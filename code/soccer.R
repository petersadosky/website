#########
# Code Summary:
# 1) Load data files of soccer information
# 2) Strip out relevant data for each player position
# 3) Use ggplot2 to make rose plots
#########

###########
# Load data
# Data comes in two text files
# one with data on players from the 2010 World Cup
# one on team level data
player <- read.table("~/2010PlayerData.txt")
team <- read.table("~/2010TeamData.txt")
############


############
# Strip out relevant information from the input files
best.teams <- team[c(10,18,29,32),]
germany <- which(as.character(player[,2]) == rownames(best.teams)[1])
netherlands <- which(as.character(player[,2]) == rownames(best.teams)[2])
spain <- which(as.character(player[,2]) == rownames(best.teams)[3])
uruguay <- which(as.character(player[,2]) == rownames(best.teams)[4])
germany.goalie <- which(player[germany,3] == "Goalkeeper")
germany.saves <- player[germany[germany.goalie],8]

netherlands.goalie <- which(player[netherlands,3] == "Goalkeeper")
netherlands.saves <- player[netherlands[netherlands.goalie],8]
spain.goalie <- which(player[spain,3] == "Goalkeeper")
spain.saves <- player[spain[spain.goalie],8]
uruguay.goalie <- which(player[uruguay,3] == "Goalkeeper")
uruguay.saves <- player[uruguay[uruguay.goalie],8]
############


############
# Follow this section of code for each player, altering the y value
# for each position on the field
# Defender #1
d.germany <- player[which(player[,2] == "Germany"),]
germany.defenders <- d.germany[which(d.germany[,3] == "Defender"),]
d1.germany <- germany.defenders[which(germany.defenders[,4] == max(germany.defenders[,4]))[1],7] ##check count
d.netherlands <- player[which(player[,2] == "Netherlands"),]
netherlands.defenders <- d.netherlands[which(d.netherlands[,3] == "Defender"),]
d1.netherlands <- netherlands.defenders[which(netherlands.defenders[,4] == max(netherlands.defenders[,4]))[1],7] ##check count at this step
d.spain <- player[which(player[,2] == "Spain"),]
spain.defenders <- d.spain[which(d.spain[,3] == "Defender"),]
d1.spain <- spain.defenders[which(spain.defenders[,4] == max(spain.defenders[,4]))[1],7]
d.uruguay <- player[which(player[,2] == "Uruguay"),]
uruguay.defenders <- d.uruguay[which(d.uruguay[,3] == "Defender"),]
d1.uruguay <- uruguay.defenders[which(uruguay.defenders[,4] == max(uruguay.defenders[,4]))[1],7]
dat <- c(rep("Germany",d1.germany),rep("Netherlands",d1.netherlands),rep("Spain",d1.spain),rep("Uruguay",d1.uruguay))
smp <- ggplot(data.frame(dat), aes(x=factor(dat),fill=factor(dat))) +
geom_bar(width=1)
smp +coord_polar() + opts(axis.line=theme_blank(),axis.text.x=theme_blank(),
 axis.text.y=theme_blank(),
 axis.title.x=theme_blank(),
 axis.title.y=theme_blank(),legend.position="none",
 panel.background=theme_blank())
############

# and then in photoshop overlay each rose plot on a diagram of soccer field
# and then add text captions