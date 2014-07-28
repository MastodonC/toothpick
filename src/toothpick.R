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
cycling.frequency<-read.xls("/Users/francinebennett/Dropbox/datasets/local-area-walking-and-cycling/cw0111.xls",skip=7)
cycling.frequency$LA.name<-paste(cycling.frequency$X,cycling.frequency$X.1,sep="")
cycling.frequency<-subset(cycling.frequency,!is.na("X1.x.per.week") & LA.name!="") 
cycling.frequency<-cycling.frequency[,c("LA.code","LA.name","X1.x.per.week")] ## Use pct of people who cycle at least once per week

walking.frequency<-read.xls("/Users/francinebennett/Dropbox/datasets/local-area-walking-and-cycling/cw0121.xls",skip=7)
walking.frequency$LA.name<-paste(walking.frequency$X,walking.frequency$X.1,sep="")
walking.frequency<-subset(walking.frequency,!is.na("X3.x.per.week") & LA.name!="") 
walking.frequency<-walking.frequency[,c("LA.code","LA.name","X3.x.per.week")] ## Use pct of people who walk at least 3 times per week

borough.healthscore<-merge(cycling.frequency,walking.frequency)

borough.healthscore$cycling.rank<-rank(borough.healthscore$X1.x.per.week)
borough.healthscore$walking.rank<-rank(borough.healthscore$X3.x.per.week)
names(borough.healthscore)<-c("LA.code","LA.name","pct_cycle_weekly","pct_walk_thriceweekly","cycling.rank","walking.rank")
borough.healthscore$LA.name<-as.character(borough.healthscore$LA.name)
borough.healthscore$LA.name<-gsub("Medway","Medway Towns",borough.healthscore$LA.name)
borough.healthscore$LA.name<-toupper(borough.healthscore$LA.name)
borough.healthscore$LA.name<-gsub(", CITY OF","",borough.healthscore$LA.name)
borough.healthscore$LA.name<-gsub(", COUNTY OF","",borough.healthscore$LA.name)

## GREEN SPACE VISIT DATA
## Read green space visit data from http://www.naturalengland.org.uk/ourwork/evidence/mene.aspx#year4
mene.survey<-read.csv("/Users/francinebennett/Dropbox/datasets/MENE CSV Respondent based data.csv")
#mene.survey<-subset(mene.survey,year=="Y1213") # Filter to most recent data. Commented out as it makes sample size very small
mene.borough<-aggregate(mene.survey$q1,by=list(mene.survey$RESIDENCE_LOCALAUTHORITY),FUN=mean) # Find average by borough. Chosen mean rather than median as an average, since median almost always ends up as 0
names(mene.borough)<-c("LA.name",y="weekly_greenspace_visits")
mene.borough$LA.name<-as.character(mene.borough$LA.name)
mene.borough$LA.name<-gsub("&","AND",mene.borough$LA.name) # Make borough names match exactly
mene.borough$LA.name<-gsub(", CITY OF","",mene.borough$LA.name)
mene.borough$LA.name<-gsub(", COUNTY OF","",mene.borough$LA.name)
mene.borough<-subset(mene.borough,LA.name!="0")
mene.borough$greenspace.rank<-rank(mene.borough$weekly_greenspace_visits)

borough.healthscore<-merge(borough.healthscore,mene.borough,all.x=TRUE)



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

## GP SURGERY AND QUALITY DATA
## Patient experience of being able to see a doctor fairly quickly P01146
## Filtered from original file using a terminal command 
## grep "P01146" results.csv > patient_experience.csv
patient.experience<-read.csv("/Users/francinebennett/Dropbox/datasets/GPOutcomes/patient_experience.csv",header=FALSE)
patient.experience<-subset(patient.experience,V3 %in% c("Able to see a doctor fairly quickly - total responses","Able to see a doctor fairly quickly - Yes"))
patient.experience<-patient.experience[,c("V1","V3","V4")]
patient.experience<-cast(patient.experience,V1~V3)
patient.experience$pct_quick<-patient.experience[,3]/patient.experience[,2]


