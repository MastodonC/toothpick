setwd("/Users/francinebennett/Desktop/client/toothpick/")
require("gdata")
require("ggplot2")
require("zoo")
require("reshape")
require("foreign")
require("plyr")
require("lubridate")

## CYCLING AND WALKING DATA
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
names(activity.frequency)<-c("LA.code","LA.name","pct_cycle_weekly","pct_walk_thriceweekly","cycling.rank","walking.rank")

## GP SURGERY AND QUALITY DATA
## Patient experience of being able to see a doctor fairly quickly P01146
## Filtered from original file using a terminal command 
## grep "P01146" /Users/francinebennett/Desktop/client/toothpick/datasets/GPOutcomes/results.csv > /Users/francinebennett/Desktop/client/toothpick/datasets/GPOutcomes/patient_experience.csv
patient.experience<-read.csv("/Users/francinebennett/Dropbox/datasets/GPOutcomes/patient_experience.csv",header=FALSE)
patient.experience<-subset(patient.experience,V3 %in% c("Able to see a doctor fairly quickly - total responses","Able to see a doctor fairly quickly - Yes"))
patient.experience<-patient.experience[,c("V1","V3","V4")]
patient.experience<-cast(patient.experience,V1~V3)
patient.experience$pct_quick<-patient.experience[,3]/patient.experience[,2]

## GREEN SPACE VISIT DATA
## Read green space visit data from http://www.naturalengland.org.uk/ourwork/evidence/mene.aspx#year4
## Waiting for response on whether this is usable under OGL
## mene.survey<-read.csv("./datasets/MENE CSV Respondent based data.csv")

## AIR QUALITY DATA
## Read air quality data, downloaded from Defra
all.files<-list.files(path = "./datasets/uk-air/boroughs", pattern = NULL, all.files = FALSE,                                              
                      full.names = TRUE, recursive = TRUE,
                      ignore.case = FALSE, include.dirs = FALSE)
no2.data<-as.data.frame(all.files)
no2.data$value<-NA

get.no2<-function(filename){ 
  temp<-read.csv(filename,skip=4,stringsAsFactors=FALSE)
  temp$Nitrogen.dioxide<-as.numeric(temp$Nitrogen.dioxide)
  no2.value<-median(temp$Nitrogen.dioxide,na.rm=TRUE)
  return(no2.value)
}
  
for(i in 1:length(all.files)){
  try(no2.data[i,2]<-get.no2(all.files[i]))
}



