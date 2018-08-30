library(dplyr)
library(tidyr)
library(ggplot2)
nba<-cbind(NBA_Team_Annual_Attendance,nba_wins_II)
nba<-nba[,c(1,2,3,12,5,6,4,7,8,9,10,11,12)]
View(nba_capstone)
write.csv(nba,"nba_capstone.csv")
str(nba_capstone)
str(nba)
View(NBA)
write.csv(nba,"NBA.csv")
nba<-nba_capstone %>%
  rename(Year = `Starting Year`,
         HomeAvgAttendance= `Home: Avg Attendance`)
ggplot(NBA,aes(x=Wins,y=HomeAvgAttendance,color=Team))+
  geom_point()
cor(NBA$Wins,NBA$HomeAvgAttendance)
knicks<-NBA[NBA$Team %in% c("NY Knicks"),]
east<-NBA[NBA$Team %in% c("Bulls","Cavaliers","Raptors","NY Knicks","Heat",
                          "Celtics","Wizards","Magic","Hornets","Pacers",
                          "Hawks","Pistons","Bucks","Nets","76ers"),]
west<-NBA[NBA$Team %in% c("Mavericks","Warriors","Trail Blazers","Jazz",
                          "Clippers","Lakers","Spurs","Thunder","Rockets",
                          "Kings","Suns","Pelicans","Grizzlies","Timberwolves",
                          "Nuggets"),]
Year2015<-NBA[NBA$Year %in% c("2015"),]
ggplot(knicks,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()
ggplot(Year2015,aes(x=Wins,y=HomeAvgAttendance,color=Team))+
  geom_point()
cor(Year2015$Wins,Year2015$HomeAvgAttendance)