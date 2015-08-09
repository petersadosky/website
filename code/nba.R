# I am interested in studying the effects of having all-star players on the regular season records and playoff results of NBA teams.


  
# Start by ripping data from basketball reference (BR)
allstar.data <- readLines("http://www.basketball-reference.com/allstar/NBA_2014.html")



# Strip out player and team data
players <- regmatches(allstar.data, regexpr("[A-Za-z]{1,20} [A-Za-z]{1,20}</a></t", allstar.data))
players <- substr(players, 1, nchar(players)-7)
allstar.teams <- regmatches(allstar.data, regexpr(">[A-Z]{3}</a", allstar.data))
allstar.teams <- substr(allstar.teams, 2, nchar(allstar.teams)-3)



# Rip team name info from wikipedia
team.data <- readLines("http://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations")
team.abbrevs <- regmatches(team.data, regexpr("d>[A-Z]{3}", team.data))
team.abbrevs <- substr(team.abbrevs, 3, nchar(team.abbrevs))
team.names <- regmatches(team.data, regexpr("([A-Z][a-z]{1,15} [A-Z][a-z]{1,15} [A-Z][a-z]{1,15}</a></td>)|([A-Z][a-z]{1,15} ([A-Z]|[0-9]{2})[a-z]{1,15}</a></td>)", team.data))
team.names <- substr(team.names, 1, nchar(team.names)-9)



# Match team names with their abbreviations, eg Cleveland = CLE
team.match <- data.frame(abbrev=team.abbrevs, team=team.names)



# Does a team have an allstar?
has.1.allstar <- sapply(team.abbrevs, function(x) ifelse(x %in% allstar.teams, 1, 0), USE.NAMES=FALSE)



# Two allstars?
has.2.allstar <- sapply(team.abbrevs, function(x) ifelse(x %in% allstar.teams[duplicated(allstar.teams)], 1, 0))



# Three?
has.3.allstar <- sapply(team.abbrevs, function(x) ifelse(x %in% names(which(table(allstar.teams)==3)), 1, 0))



# How many allstars on each team?
num.allstar <- rowSums(cbind(has.1.allstar, has.2.allstar, has.3.allstar))



# Rip win/loss data from BR
win.data <- readLines("http://www.basketball-reference.com/leagues/NBA_2014.html")
teams <- win.data[grep("<td align=\"right\" >[0-9]{2}</td>", win.data)[seq(1, 60, 2)]-1]
teams <- gsub("<|>", "", substr(teams, 63, nchar(teams)-50))
teams[13] <- "Charlotte Hornets" # team name didn't match for CLT, Bobcats == Hornets
matching <- match(team.match$team, teams)
team.wins <- regmatches(win.data, regexpr("<td align=\"right\" >[0-9]{2}</td>", win.data))[seq(1, 60, 2)]
team.wins <- as.numeric(substr(team.wins, 20, nchar(team.wins)-5))[matching]



# Combine all data into data frame
nba <- data.frame(team=team.abbrevs, wins=team.wins, has.1.allstar, has.2.allstar, has.3.allstar, num.allstar, stringsAsFactors=FALSE)



# Initial linear regressions
summary(lm(wins~has.1.allstar+has.2.allstar+has.3.allstar+num.allstar, data=nba))
summary(lm(wins~has.1.allstar+has.2.allstar, data=nba))  
summary(lm(wins~has.1.allstar, data=nba))
summary(lm(wins~has.2.allstar, data=nba)) 
summary(lm(wins~has.3.allstar, data=nba)) 
summary(lm(wins~num.allstar, data=nba))



# rip NBA player salaries
# I couldn't find data on 2013-14 online since it seems to have disappeared from BR, but it's still on archive.org
download.file("https://web.archive.org/web/20131029022900/http://www.basketball-reference.com/contracts/players.html", destfile=paste0(getwd(), "/salaries.html"), method="curl", quiet=TRUE)
salaries <- readLines(paste0(getwd(), "/salaries.html"))
file.remove(paste0(getwd(), "/salaries.html"))



# Weird/unconstant row indices needed from BR, poorly written loop here but gets job done
rows <- c()
k <- 0
start <- 463
while (k == 0) {
	sequ <- seq(start, start+247, 13)
	rows <- c(rows, sequ)
	start <- start+278
	if (start > 7413) {
		k <- 1
	}
}



# Get salary info
salary <- salaries[rows]
salary <- as.numeric(gsub("\\$|>|\"", "", substr(salary, 28, nchar(salary)-15)))
salary <- salary[!is.na(salary)]



allstar.salaries <- unlist(sapply(players, function(x) grep(x, salaries)))+3
allstar.salaries <- allstar.salaries[-15]
allstar.salaries <- salaries[allstar.salaries]
allstar.salaries <- as.numeric(gsub(">|\\$|\"", "", substr(allstar.salaries, 28, nchar(allstar.salaries)-15)))



mean(allstar.salaries); sd(allstar.salaries)
mean(salary); sd(salary)
wilcox.test(salary, allstar.salaries, alternative="less")
# stat significant difference between salaries



salary <- c(salary, allstar.salaries)



# Start making graphs
library(ggplot2)
library(scales)
player <- c(rep("Non-All Stars", length(salary)-24), rep("NBA All Stars", length(allstar.salaries)))
sal <- data.frame(player, salary)



# Save density curve graph as png file
png("nba-salaries.png", res=150)
p <- ggplot(sal, aes(salary, fill=player)) + geom_density(alpha=0.4) + labs(x="Salary ($)", y="Density") + theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), legend.title=element_blank(), legend.position="top")
dev.off()
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}



# graph 2
wins.0.allstar <- nba$wins[which(nba$has.1.allstar == 0)]
wins.1.allstar <- nba$wins[which(nba$has.1.allstar == 1)]
# are these dists normally distributed?
plot(density(wins.0.allstar))
lines(density(wins.1.allstar), col=2)
# yes, pretty normal, so use t test
t.test(wins.0.allstar, wins.1.allstar, alternative="l", conf.level=0.99)



# Save boxplot as png file
wins.df <- data.frame(star=c(rep("Yes", length(wins.1.allstar)), rep("No", length(wins.0.allstar))), wins=c(wins.1.allstar, wins.0.allstar))
png("../documents/website/images/allstar-wins.png", res=150)
ggplot(wins.df, aes(star, wins)) + geom_boxplot(aes(fill=star, alpha=0.4)) + geom_jitter() + theme(legend.position="none") + labs(x="", y="Regular Season Wins", title="Team has All Star?") + scale_fill_manual(values=c(gg_color_hue(2)[2], gg_color_hue(1)[1]))
dev.off()



# Rip playoff data from BR
playoffs <- readLines("http://www.basketball-reference.com/leagues/NBA_2014.html")
playoff.results <- grep("</a> over", playoffs, value=TRUE)
playoff.sched <- substr(playoff.results, 1, nchar(playoff.results)-79)
playoff.teams <- gsub(">|<|/", " ", playoff.sched)
playoff.sched2 <- substr(playoff.results, 36, nchar(playoff.results)-79)
playoff.teams2 <- gsub(">|<|/", " ", playoff.sched2)



winners <- regmatches(playoff.teams, regexpr("[A-Z]{3}", playoff.teams))
losers <- regmatches(playoff.teams2, regexpr("[A-Z]{3}", playoff.teams2))



winner.counts <- table(winners)
loser.counts <- table(losers)
names(winner.counts)[1] <- "BKN"; names(loser.counts)[2] <- "BKN" #not BRK
team.playoff.wins <- 
nba$playoff.wins <- rep(0, nrow(nba))
nba$playoff.wins[match(names(winner.counts), nba$team)] <- winner.counts


 
# Testing linear regressions
summary(lm(playoff.wins~has.1.allstar, nba))
summary(lm(playoff.wins~num.allstar, nba))



new.lab <- nba$playoff.wins



nba$has.1.allstar.f <- ifelse(nba$has.1.allstar == 1, "Yes", "No")



# Save scatterplot as png file
png("~/documents/website/images/playoff-wins.png", res=150)
ggplot(nba, aes(has.1.allstar.f, playoff.wins)) + geom_point(shape=0, size=5, aes(color=gg_color_hue(2)[(nba$has.1.allstar+1)])) + geom_jitter(size=1.2, position=position_jitter(width=0.1, height=0.1)) + labs(x="", y="Playoff Series Wins", title="Team Has All-Star?") + theme(legend.position="top")
dev.off()