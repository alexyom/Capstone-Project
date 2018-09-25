library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
##initial data wrangling 
nba<-cbind(NBA_Team_Annual_Attendance,nba_wins_II)
nba<-nba[,c(1,2,3,12,5,6,4,7,8,9,10,11)]
write.csv(nba,"nba_capstone.csv")
write.csv(nba,"NBA.csv")
nba<-nba_capstone %>%
  rename(Year = `Starting Year`,
         HomeAvgAttendance= `Home: Avg Attendance`)

##introductory plotting of data
ggplot(NBA,aes(x=Wins,y=HomeAvgAttendance,color=Team))+
  geom_point()+
  labs(title="NBA Statistics from 2000-2015")
cor(NBA$Wins,NBA$HomeAvgAttendance)
ggplot(NBA,aes(Year,y=HomeAvgAttendance))+
  geom_point()+
  labs(title="Home Team Attendance")
cor(NBA$Wins,NBA$HomeAvgAttendance)


##Year
Year2015<-NBA[NBA$Year %in% c("2015"),]
ggplot(Year2015,aes(x=Wins,y=HomeAvgAttendance,color=Team))+
  geom_point()+
  geom_text_repel(aes(label=Team))+
  labs(title="Year 2015")
cor(Year2015$Wins,Year2015$HomeAvgAttendance)
mean(Year2015$HomeAvgAttendance)
Year2015$Teams<-Year2015$Team
Year2015$Avg<-round(Year2015$HomeAvgAttendance-mean(Year2015$HomeAvgAttendance))
Year2015$Attendance<-ifelse(Year2015$HomeAvgAttendance < 17849,"below", "above")
Year2015<-Year2015[order(-Year2015$Rank),]
Year2015$Teams<-factor(Year2015$Teams,levels=Year2015$Teams)
ggplot(Year2015,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2015-2016 Season")+
  ylab("Deviation from League Average")+
  coord_flip()

Year2014<-NBA[NBA$Year %in% c("2014"),]
cor(Year2014$Wins,Year2014$HomeAvgAttendance)
mean(Year2014$HomeAvgAttendance)
Year2014$Teams<-Year2014$Team
Year2014$Avg<-round(Year2014$HomeAvgAttendance-mean(Year2014$HomeAvgAttendance))
Year2014$Attendance<-ifelse(Year2014$HomeAvgAttendance > 17809,"above", "below")
Year2014<-Year2014[order(-Year2014$Rank),]
Year2014$Teams<-factor(Year2014$Teams,levels=Year2014$Teams)
ggplot(Year2014,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2014-2015 Season")+
  ylab("Deviation from League Average")+
  coord_flip()

Year2013<-NBA[NBA$Year %in% c("2013"),]
cor(Year2013$Wins,Year2013$HomeAvgAttendance)
mean(Year2013$HomeAvgAttendance)
Year2013$Teams<-Year2013$Team
Year2013$Avg<-round(Year2013$HomeAvgAttendance-mean(Year2013$HomeAvgAttendance))
Year2013$Attendance<-ifelse(Year2013$HomeAvgAttendance > 17407,"above", "below")
Year2013<-Year2013[order(-Year2013$Rank),]
Year2013$Teams<-factor(Year2013$Teams,levels=Year2013$Teams)
ggplot(Year2013,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2013-2014 Season")+
  ylab("Deviation from League Average")+
  coord_flip()

Year2012<-NBA[NBA$Year %in% c("2012"),]
mean(Year2012$HomeAvgAttendance)
Year2012$Teams<-Year2012$Team
Year2012$Avg<-round(Year2012$HomeAvgAttendance-mean(Year2012$HomeAvgAttendance))
Year2012$Attendance<-ifelse(Year2012$Avg > 0,"above", "below")
Year2012<-Year2012[order(-Year2012$Rank),]
Year2012$Teams<-factor(Year2012$Teams,levels=Year2012$Teams)
ggplot(Year2012,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2012-2013 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2012$Wins,Year2012$HomeAvgAttendance)


Year2011<-NBA[NBA$Year %in% c("2011"),]
mean(Year2011$HomeAvgAttendance)
Year2011$Teams<-Year2011$Team
Year2011$Avg<-round(Year2011$HomeAvgAttendance-mean(Year2011$HomeAvgAttendance))
Year2011$Attendance<-ifelse(Year2011$Avg > 0,"above", "below")
Year2011<-Year2011[order(-Year2011$Rank),]
Year2011$Teams<-factor(Year2011$Teams,levels=Year2011$Teams)
ggplot(Year2011,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2011-2012 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2011$Wins,Year2011$HomeAvgAttendance)


Year2010<-NBA[NBA$Year %in% c("2010"),]
mean(Year2010$HomeAvgAttendance)
Year2010$Teams<-Year2010$Team
Year2010$Avg<-round(Year2010$HomeAvgAttendance-mean(Year2010$HomeAvgAttendance))
Year2010$Attendance<-ifelse(Year2010$Avg > 0,"above", "below")
Year2010<-Year2010[order(-Year2010$Rank),]
Year2010$Teams<-factor(Year2010$Teams,levels=Year2010$Teams)
ggplot(Year2010,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2010-2011 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2010$Wins,Year2010$HomeAvgAttendance)


Year2009<-NBA[NBA$Year %in% c("2009"),]
mean(Year2009$HomeAvgAttendance)
Year2009$Teams<-Year2009$Team
Year2009$Avg<-round(Year2009$HomeAvgAttendance-mean(Year2009$HomeAvgAttendance))
Year2009$Attendance<-ifelse(Year2009$Avg > 0,"above", "below")
Year2009<-Year2009[order(-Year2009$Rank),]
Year2009$Teams<-factor(Year2009$Teams,levels=Year2009$Teams)
ggplot(Year2009,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2009-2010 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2009$Wins,Year2009$HomeAvgAttendance)


Year2008<-NBA[NBA$Year %in% c("2008"),]
mean(Year2008$HomeAvgAttendance)
Year2008$Teams<-Year2008$Team
Year2008$Avg<-round(Year2008$HomeAvgAttendance-mean(Year2008$HomeAvgAttendance))
Year2008$Attendance<-ifelse(Year2008$Avg > 0,"above", "below")
Year2008<-Year2008[order(-Year2008$Rank),]
Year2008$Teams<-factor(Year2008$Teams,levels=Year2008$Teams)
ggplot(Year2008,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2008-2009 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2008$Wins,Year2008$HomeAvgAttendance)


Year2007<-NBA[NBA$Year %in% c("2007"),]
mean(Year2007$HomeAvgAttendance)
Year2007$Teams<-Year2007$Team
Year2007$Avg<-round(Year2007$HomeAvgAttendance-mean(Year2007$HomeAvgAttendance))
Year2007$Attendance<-ifelse(Year2007$Avg > 0,"above", "below")
Year2007<-Year2007[order(-Year2007$Rank),]
Year2007$Teams<-factor(Year2007$Teams,levels=Year2007$Teams)
ggplot(Year2007,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2007-2008 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2007$Wins,Year2007$HomeAvgAttendance)


Year2006<-NBA[NBA$Year %in% c("2006"),]
mean(Year2006$HomeAvgAttendance)
Year2006$Teams<-Year2006$Team
Year2006$Avg<-round(Year2006$HomeAvgAttendance-mean(Year2006$HomeAvgAttendance))
Year2006$Attendance<-ifelse(Year2006$Avg > 0,"above", "below")
Year2006<-Year2006[order(-Year2006$Rank),]
Year2006$Teams<-factor(Year2006$Teams,levels=Year2006$Teams)
ggplot(Year2006,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2006-2007 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2006$Wins,Year2006$HomeAvgAttendance)
ggplot(Year2006,aes(x=Wins,y=HomeAvgAttendance,color=Team))+
  geom_point()+
  geom_text_repel(aes(label=Team))+
  labs(title="Year 2006")


Year2005<-NBA[NBA$Year %in% c("2005"),]
mean(Year2005$HomeAvgAttendance)
Year2005$Teams<-Year2005$Team
Year2005$Avg<-round(Year2005$HomeAvgAttendance-mean(Year2005$HomeAvgAttendance))
Year2005$Attendance<-ifelse(Year2005$Avg > 0,"above", "below")
Year2005<-Year2005[order(-Year2005$Rank),]
Year2005$Teams<-factor(Year2005$Teams,levels=Year2005$Teams)
ggplot(Year2005,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2005-2006 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2005$Wins,Year2005$HomeAvgAttendance)


Year2004<-NBA[NBA$Year %in% c("2004"),]
mean(Year2004$HomeAvgAttendance)
Year2004$Teams<-Year2004$Team
Year2004$Avg<-round(Year2004$HomeAvgAttendance-mean(Year2004$HomeAvgAttendance))
Year2004$Attendance<-ifelse(Year2004$Avg > 0,"above", "below")
Year2004<-Year2004[order(-Year2004$Rank),]
Year2004$Teams<-factor(Year2004$Teams,levels=Year2004$Teams)
ggplot(Year2004,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2004-2005 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2004$Wins,Year2004$HomeAvgAttendance)


Year2003<-NBA[NBA$Year %in% c("2003"),]
mean(Year2003$HomeAvgAttendance)
Year2003$Teams<-Year2003$Team
Year2003$Avg<-round(Year2003$HomeAvgAttendance-mean(Year2003$HomeAvgAttendance))
Year2003$Attendance<-ifelse(Year2003$Avg > 0,"above", "below")
Year2003<-Year2003[order(-Year2003$Rank),]
Year2003$Teams<-factor(Year2003$Teams,levels=Year2003$Teams)
ggplot(Year2003,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2003-2004 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2003$Wins,Year2003$HomeAvgAttendance)
ggplot(Year2003,aes(x=Wins,y=HomeAvgAttendance,color=Team))+
  geom_point()+
  geom_text_repel(aes(label=Team))+
  labs(title="Year 2003")


Year2002<-NBA[NBA$Year %in% c("2002"),]
mean(Year2002$HomeAvgAttendance)
Year2002$Teams<-Year2002$Team
Year2002$Avg<-round(Year2002$HomeAvgAttendance-mean(Year2002$HomeAvgAttendance))
Year2002$Attendance<-ifelse(Year2003$Avg > 0,"above", "below")
Year2002<-Year2002[order(-Year2002$Rank),]
Year2002$Teams<-factor(Year2002$Teams,levels=Year2002$Teams)
ggplot(Year2002,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2002-2003 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2002$Wins,Year2002$HomeAvgAttendance)


Year2001<-NBA[NBA$Year %in% c("2001"),]
mean(Year2001$HomeAvgAttendance)
Year2001$Teams<-Year2001$Team
Year2001$Avg<-round(Year2001$HomeAvgAttendance-mean(Year2001$HomeAvgAttendance))
Year2001$Attendance<-ifelse(Year2001$Avg > 0,"above", "below")
Year2001<-Year2001[order(-Year2001$Rank),]
Year2001$Teams<-factor(Year2001$Teams,levels=Year2001$Teams)
ggplot(Year2001,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2001-2002 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2001$Wins,Year2001$HomeAvgAttendance)


Year2000<-NBA[NBA$Year %in% c("2000"),]
mean(Year2000$HomeAvgAttendance)
Year2000$Teams<-Year2000$Team
Year2000$Avg<-round(Year2000$HomeAvgAttendance-mean(Year2000$HomeAvgAttendance))
Year2000$Attendance<-ifelse(Year2000$Avg > 0,"above", "below")
Year2000<-Year2000[order(-Year2000$Rank),]
Year2000$Teams<-factor(Year2000$Teams,levels=Year2000$Teams)
ggplot(Year2000,aes(x=Teams,y=Avg,label=Avg))+
  geom_bar(stat='identity',aes(fill=Wins),width=.5)+
  labs(title="Home Team Attendance Compared to League Average",
       subtitle="2000-2001 Season")+
  ylab("Deviation from League Average")+
  coord_flip()
cor(Year2001$Wins,Year2001$HomeAvgAttendance)



##Team
knicks<-NBA[NBA$Team %in% c("NY Knicks"),]
ggplot(knicks,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="New York Knicks",
       subtitle="The correlation is -.02 ")
cor(knicks$Wins,knicks$HomeAvgAttendance)

warriors<-NBA[NBA$Team %in% c("Warriors"),]
cor(warriors$Wins,warriors$HomeAvgAttendance)
ggplot(warriors,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Golden State Warriors",
       subtitle="The correlation is .63 ")

lakers<-NBA[NBA$Team %in% c("Lakers"),]
ggplot(lakers,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Los Angeles Lakers",
       subtitle="The correlation is .59 ")
cor(lakers$Wins,lakers$HomeAvgAttendance)

hawks<-NBA[NBA$Team %in% c("Hawks"),]
cor(hawks$Wins,hawks$HomeAvgAttendance)
ggplot(hawks,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Atlanta Hawks",
       subtitle="The correlation is .67 ")

bulls<-NBA[NBA$Team %in% c("Bulls"),]
cor(bulls$Wins,bulls$HomeAvgAttendance)
ggplot(bulls,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Chicago Bulls",
       subtitle="The correlation is .54 ")

nuggets<-NBA[NBA$Team %in% c("Nuggets"),]
cor(nuggets$Wins,nuggets$HomeAvgAttendance)
ggplot(nuggets,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Denver Nuggets",
       subtitle="The correlation is .80 ")

cavs<-NBA[NBA$Team %in% c("Cavaliers"),]
cor(cavs$Wins,cavs$HomeAvgAttendance)
ggplot(cavs,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Cleveland Cavaliers",
       subtitle="The correlation is .77 ")

mavs<-NBA[NBA$Team %in% c("Mavericks"),]
cor(mavs$Wins,mavs$HomeAvgAttendance)
ggplot(mavs,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Dallas Mavericks",
       subtitle="The correlation is -.06 ")

raps<-NBA[NBA$Team %in% c("Raptors"),]
cor(raps$Wins,raps$HomeAvgAttendance)
ggplot(raps,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Toronto Raptors",
       subtitle="The correlation is .70 ")

heat<-NBA[NBA$Team %in% c("Heat"),]
cor(heat$Wins,heat$HomeAvgAttendance)
ggplot(heat,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Miami Heat",
       subtitle="The correlation is .40 ")

trailblazers<-NBA[NBA$Team %in% c("Trail Blazers"),]
cor(trailblazers$Wins,trailblazers$HomeAvgAttendance)
ggplot(trailblazers,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Portland Trailblazers",
       subtitle="The correlation is .67 ")

jazz<-NBA[NBA$Team %in% c("Jazz"),]
cor(jazz$Wins,jazz$HomeAvgAttendance)
ggplot(jazz,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Utah Jazz",
       subtitle="The correlation is .65 ")

clippers<-NBA[NBA$Team %in% c("Clippers"),]
cor(clippers$Wins,clippers$HomeAvgAttendance)
ggplot(clippers,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Los Angeles Clippers",
       subtitle="The correlation is .69 ")

spurs<-NBA[NBA$Team %in% c("Spurs"),]
cor(spurs$Wins,spurs$HomeAvgAttendance)
ggplot(spurs,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="San Antonio Spurs",
       subtitle="The correlation is .05 ")
  
celtics<-NBA[NBA$Team %in% c("Celtics"),]
cor(celtics$Wins,celtics$HomeAvgAttendance)
ggplot(celtics,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Boston Celtics",
       subtitle="The correlation is .42 ")
  
thunder<-NBA[NBA$Team %in% c("Thunder"),]
cor(thunder$Wins,thunder$HomeAvgAttendance)
ggplot(thunder,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Oklahoma City Thunder",
       subtitle="The correlation is .56 ")

rockets<-NBA[NBA$Team %in% c("Rockets"),]
cor(rockets$Wins,rockets$HomeAvgAttendance)
ggplot(rockets,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Houston Rockets",
       subtitle="The correlation is .66 ") 

wiz<-NBA[NBA$Team %in% c("Wizards"),]
cor(wiz$Wins,wiz$HomeAvgAttendance)
ggplot(wiz,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Washington Wizards",
       subtitle="The correlation is .55 ")   

magic<-NBA[NBA$Team %in% c("Magic"),]
cor(magic$Wins,magic$HomeAvgAttendance)
ggplot(magic,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Orlando Magic",
       subtitle="The correlation is .25 ")  

hornets<-NBA[NBA$Team %in% c("Hornets"),]
cor(hornets$Wins,hornets$HomeAvgAttendance)
ggplot(hornets,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Charlotte Hornets",
       subtitle="The correlation is .52 ")  
  
kings<-NBA[NBA$Team %in% c("Kings"),]
cor(kings$Wins,kings$HomeAvgAttendance)
ggplot(kings,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Sacramento Kings",
       subtitle="The correlation is .74 ")  
  
suns<-NBA[NBA$Team %in% c("Suns"),]
cor(suns$Wins,suns$HomeAvgAttendance)
ggplot(suns,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Phoenix Suns",
       subtitle="The correlation is .68 ")    

pacers<-NBA[NBA$Team %in% c("Pacers"),]
cor(pacers$Wins,pacers$HomeAvgAttendance)
ggplot(pacers,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Indiana Pacers",
       subtitle="The correlation is .53 ")  
  
pelicans<-NBA[NBA$Team %in% c("Pelicans"),]
cor(pelicans$Wins,pelicans$HomeAvgAttendance)
ggplot(pelicans,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="New Orleans Pelicans",
       subtitle="The correlation is .01 ")    

grizz<-NBA[NBA$Team %in% c("Grizzlies"),]
cor(grizz$Wins,grizz$HomeAvgAttendance)
ggplot(grizz,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Memphis Grizzlies",
       subtitle="The correlation is .77 ")   
  
pistons<-NBA[NBA$Team %in% c("Pistons"),]
cor(pistons$Wins,pistons$HomeAvgAttendance)
ggplot(pistons,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Detroit Pistons",
       subtitle="The correlation is .83 ")   

bucks<-NBA[NBA$Team %in% c("Bucks"),]
cor(bucks$Wins,bucks$HomeAvgAttendance)
ggplot(bucks,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Milwaukee Bucks",
       subtitle="The correlation is .57 ")   

nets<-NBA[NBA$Team %in% c("Nets"),]
cor(nets$Wins,nets$HomeAvgAttendance)
ggplot(nets,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text(aes(label=Year))+
  labs(title="Brooklyn Nets",
       subtitle="The correlation is .56 ")   
  
sixers<-NBA[NBA$Team %in% c("76ers"),]
cor(sixers$Wins,sixers$HomeAvgAttendance)
ggplot(sixers,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Philadelphia 76ers",
       subtitle="The correlation is .67 ")   

twolves<-NBA[NBA$Team %in% c("Timberwolves"),]
cor(twolves$Wins,twolves$HomeAvgAttendance)
ggplot(twolves,aes(x=Wins,y=HomeAvgAttendance,color=Year))+
  geom_point()+
  geom_text_repel(aes(label=Year))+
  labs(title="Minnesota Timberwolves",
       subtitle="The correlation is .65 ")     