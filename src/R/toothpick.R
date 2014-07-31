## TOOTHPICK SCRIPT

## - ingests and rolls up various health datasets to borough level
## - ranks each metric per borough (normalised rank centred on zero)
## - creates overall rank by taking means of individual metric ranks
## - outputs csv with raw metrics, metric-level ranks, and overall rank, for each borough

# Location where copy of raw data is stored
setwd("/Users/francinebennett/Dropbox/datasets/") 

# Libraries used in this analysis
require("gdata")
require("ggplot2")
require("zoo")
require("reshape")
require("foreign")
require("plyr")
require("lubridate")

## CYCLING AND WALKING DATA
## Use cycling and walking frequency data from DfT transport mode survey at https://www.gov.uk/government/statistics/local-area-walking-and-cycling-in-england-2012-to-2013

cycling.frequency<-read.xls("local-area-walking-and-cycling/cw0111.xls",skip=7)
cycling.frequency$LA.name<-paste(cycling.frequency$X,cycling.frequency$X.1,sep="")
cycling.frequency<-subset(cycling.frequency,!is.na("X1.x.per.week") & LA.name!="") 
cycling.frequency<-cycling.frequency[,c("LA.code","LA.name","X1.x.per.week")] ## Use pct of people who cycle at least once per week
cycling.frequency$LA.code<-gsub("E07000100","E07000240",cycling.frequency$LA.code) # Update St Albans codes for post boundary changes
cycling.frequency$LA.code<-gsub("E07000104","E07000241",cycling.frequency$LA.code)
names(cycling.frequency)[3]<-"cycling_weekly"
cycling.frequency$cycling.rank<-
  scale(rank(cycling.frequency$cycling_weekly),
        center=TRUE,
        scale=FALSE) # Rank data, centre on 0 to ensure that missing boroughs don't get a skewed final score

walking.frequency<-read.xls("local-area-walking-and-cycling/cw0121.xls",skip=7)
walking.frequency$LA.name<-paste(walking.frequency$X,walking.frequency$X.1,sep="")
walking.frequency<-subset(walking.frequency,!is.na("X3.x.per.week") & LA.name!="") 
walking.frequency<-walking.frequency[,c("LA.code","LA.name","X3.x.per.week")] ## Use pct of people who walk at least 3 times per week
walking.frequency$LA.code<-gsub("E07000100","E07000240",walking.frequency$LA.code) # Update St Albans codes for post boundary changes
walking.frequency$LA.code<-gsub("E07000104","E07000241",walking.frequency$LA.code)
names(walking.frequency)[3]<-"walking_thriceweekly"
walking.frequency$walking.rank<-scale(rank(walking.frequency$walking_thriceweekly),center=TRUE,scale=FALSE)

## GREEN SPACE VISIT DATA
## Read green space visit data from http://www.naturalengland.org.uk/ourwork/evidence/mene.aspx#year4

mene.survey<-read.csv("MENE CSV Respondent based data.csv")
mene.borough<-aggregate(mene.survey$q1,by=list(mene.survey$RESIDENCE_LOCALAUTHORITY),FUN=mean) # Find average by borough. Chosen mean rather than median as an average, since median almost always ends up as 0
names(mene.borough)<-c("LA.name",y="weekly_greenspace_visits")
mene.borough$LA.name<-as.character(mene.borough$LA.name)
mene.borough$LA.name<-gsub("&","AND",mene.borough$LA.name) # Make borough names match exactly
mene.borough<-subset(mene.borough,LA.name!="0")
mene.borough$greenspace.rank<-scale(rank(mene.borough$weekly_greenspace_visits),center=TRUE,scale=FALSE)

## GP SURGERY AND QUALITY DATA
## Patient experience of being able to see a doctor fairly quickly P01146
## Filtered from original file using a terminal command 
## grep "P01146" results.csv > patient_experience.csv
patient.experience<-read.csv("GPOutcomes/patient_experience.csv",header=FALSE)
patient.experience<-subset(patient.experience,V3 %in% c("Able to see a doctor fairly quickly - total responses","Able to see a doctor fairly quickly - Yes"))
patient.experience<-patient.experience[,c("V1","V3","V4")]
patient.experience<-cast(patient.experience,V1~V3)
practice.locations<-read.csv("practice-to-borough.csv",sep="\t",header=FALSE)
patient.experience<-merge(patient.experience,practice.locations,all.x=TRUE)
names(patient.experience)<-c("practice_code","question_responses","positive_responses","LA.code")
borough.gp.experience<-ddply(patient.experience, .(LA.code), summarize, question_responses = sum(question_responses,na.rm=TRUE), positive_responses = sum(positive_responses,na.rm=TRUE))
borough.gp.experience$pct_canseegp<-borough.gp.experience$positive_responses/borough.gp.experience$question_responses
borough.gp.experience$gp.rank<-scale(rank(borough.gp.experience$pct_canseegp),center=TRUE,scale=FALSE)
borough.gp.experience<-borough.gp.experience[,c("LA.code","pct_canseegp","gp.rank")]

## HOSPITAL FRIENDS AND FAMILY DATA
hospital.ae<-read.csv("Friends and Family/FFT_AE_csv4.csv",skip=2)
hospital.ip<-read.csv("Friends and Family/FFT_IP_csv4.csv",skip=2)
hospital.maternity<-read.csv("Friends and Family/FFT_Mat_csv3.csv",skip=2)

# Filter to just the site codes and test scores. Using only A&E scores as other response rates are very los
hospital.ae<-hospital.ae[,c("Site.Code","Friends.and.Family.Test.Score")]
hospital.locations<-read.csv("hospital-locations.csv",sep="\t",header=FALSE)
hospital.ae<-merge(hospital.ae,hospital.locations,by.x="Site.Code",by.y="V1",all.x=TRUE)
borough.hospital.experience<-ddply(hospital.ae, .(V3), summarize, hospital_experience_score = sum(Friends.and.Family.Test.Score))
names(borough.hospital.experience)<-c("LA.code","hospital_experience_score")
borough.hospital.experience$hospital.rank<-scale(rank(borough.hospital.experience$hospital_experience_score),center=TRUE,scale=FALSE)

## DENTISTRY DATA
dentists<-read.csv("mastodonc 2.csv",header=TRUE)
dentist.locations<-read.csv("dentists-to-borough.csv",sep="\t",header=FALSE)
dentists<-merge(dentists,dentist.locations,by.x="id",by.y="V1",all.x=TRUE)
names(dentists)<-c("dentist_id","nhs_fees","postcode","dental_practitioners","review_score","LA.code")
dentists.borough<-ddply(dentists, .(LA.code), summarize, dental_practitioners = sum(dental_practitioners))
# Normalise count of dentists by population estimate from ONS at http://www.neighbourhood.statistics.gov.uk/dissemination/instanceSelection.do?JSAllowed=true&Function=&%24ph=60_61_60_61&CurrentPageId=61&step=2&datasetFamilyId=1813&instanceSelection=134023&Next.x=9&Next.y=5
population<-read.csv("K30A0312_2725_2011SOA_LA.CSV",skip=2,stringsAsFactors=FALSE)
population<-population[,c("X","All.Persons..All.Ages")]
population$X<-gsub("E07000100","E07000240",population$X) # Update St Albans codes for post boundary changes
population$X<-gsub("E07000104","E07000241",population$X)
population$All.Persons..All.Ages<-as.numeric(population$All.Persons..All.Ages)
dentists.borough<-merge(dentists.borough,population,by.x="LA.code",by.y="X",all.x=TRUE)
dentists.borough$dentists_per_thousand<-dentists.borough$dental_practitioners/(dentists.borough$All.Persons..All.Ages/1000)
dentists.borough<-dentists.borough[,c("LA.code","dentists_per_thousand")]
dentists.borough$dentists.rank<-scale(rank(dentists.borough$dentists_per_thousand),center=TRUE,scale=FALSE)

## MERGE ALL DATASETS AND RANKINGS TO CREATE A SINGLE SCORE
borough.healthscore<-cycling.frequency
borough.healthscore<-merge(borough.healthscore,walking.frequency,all=TRUE)
borough.healthscore$LA.name<-toupper(borough.healthscore$LA.name)

# Fix borough names so that they match greenspace naming conventions
borough.healthscore$LA.name<-gsub("MEDWAY","MEDWAY TOWNS",borough.healthscore$LA.name)
borough.healthscore$LA.name<-gsub(", CITY OF","",borough.healthscore$LA.name)
borough.healthscore$LA.name<-gsub(", COUNTY OF","",borough.healthscore$LA.name)

borough.healthscore<-merge(borough.healthscore,mene.borough,all=TRUE)
borough.healthscore<-merge(borough.healthscore,borough.hospital.experience,by="LA.code",all=TRUE)
borough.healthscore<-merge(borough.healthscore,borough.gp.experience,by="LA.code",all=TRUE)
borough.healthscore<-merge(borough.healthscore,dentists.borough,by="LA.code",all=TRUE)

borough.healthscore$overall.rank<-
  rowMeans(
    borough.healthscore[,c("cycling.rank",
                           "walking.rank",
                           "greenspace.rank",
                           "hospital.rank",
                           "gp.rank",
                           "dentists.rank")], na.rm = TRUE, dims = 1)

borough.healthscore<-subset(borough.healthscore,!is.na(LA.name) & !is.na(LA.code))
borough.healthscore<-borough.healthscore[order(borough.healthscore$overall.rank,decreasing=TRUE),]
names(borough.healthscore)<-gsub("\\.","_",names(borough.healthscore))
write.csv(borough.healthscore,"borough_scores.csv",quote=FALSE,row.names=FALSE)
