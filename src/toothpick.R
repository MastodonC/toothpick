setwd("/Users/francinebennett/Desktop/client/toothpick/")
require("gdata")
require("ggplot2")
require("zoo")
require("reshape")
require("foreign")
require("plyr")
require("lubridate")

## Use cycling and walking frequency data from DfT transport mode survey at https://www.gov.uk/government/statistics/local-area-walking-and-cycling-in-england-2012-to-2013
cycling.frequency<-read.xls("datasets/local-area-walking-and-cycling/cw0111.xls",skip=7)
cycling.frequency<-subset(cycling.frequency,Local.Authority=="")
cycling.frequency<-subset(cycling.frequency,!is.na("X1.x.per.week")) 
cycling.frequency<-subset(cycling.frequency,X!="")
cycling.frequency<-cycling.frequency[,c("LA.code","X","X1.x.per.week")] ## Use pct of people who cycle at least once per week

walking.frequency<-read.xls("datasets/local-area-walking-and-cycling/cw0121.xls",skip=7)
walking.frequency<-subset(walking.frequency,Local.Authority=="")
walking.frequency<-subset(walking.frequency,!is.na("X1.x.per.week"))
walking.frequency<-subset(walking.frequency,X!="")
walking.frequency<-walking.frequency[,c("LA.code","X","X3.x.per.week")] ## Use pct of people who walk at least 3 times per week

activity.frequency<-merge(cycling.frequency,walking.frequency)

activity.frequency$cycling.rank<-rank(activity.frequency$X1.x.per.week)
activity.frequency$walking.rank<-rank(activity.frequency$X3.x.per.week)
