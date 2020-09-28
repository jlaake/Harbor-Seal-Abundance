## This code removes anything in the workspace and then sets up data and saves in pvdf.csv.
## it is read in by data_selection.r to set date and year range for estimation code in
## HC Estimate.r  SPS&NI Estimate.r and Coastal Estimate.r
##
rm(list=ls())
library(readr)
# Uses some of the functions in functions.r
source("functions.R")

#Read in/set up raw dataset
aerial.pol <- read.csv("PolygonAerialData.csv")
aerial.pol$Day <- as.Date(aerial.pol$Day, "%d-%b-%y")

# extract pv only
aerial.pol$Species=toupper(as.character(aerial.pol$Species))
aerial.pol=aerial.pol[aerial.pol$Species=="PV",]

# remove X - record number
aerial.pol$X=NULL

# use counttype=1 only (counttype=2 is in water)
aerial.pol=aerial.pol[aerial.pol$Counttype==1,]

#remove records with NA stock
aerial.pol=aerial.pol[!is.na(aerial.pol$Stock),]

#remove records with sitecode==15.xx which are in Oregon
aerial.pol=aerial.pol[floor(aerial.pol$Sitecode)!=15,]

# add date and time as POSIXct value
aerial.pol$Tidetime <- as.POSIXct(paste(aerial.pol$Day,aerial.pol$Tidetime),format="%Y-%m-%d %I:%M:%S %p", tz="US/Pacific")
aerial.pol$Survey.time <- as.POSIXct(paste(aerial.pol$Day,aerial.pol$Survey.time),format="%Y-%m-%d %I:%M:%S %p", tz="US/Pacific")

# get TideStation values
TideStation=read.csv("LocationTideStation.csv")
TideStation=TideStation[TideStation$Tidestation!="",]
TideStation=unique(TideStation[,c("Sitecode","Tidestation")])
TideStation$Tidestation[TideStation$Tidestation==""&floor(TideStation$Sitecode)==1]="Astoria"
TideStation$Tidestation[TideStation$Tidestation==""&floor(TideStation$Sitecode)%in%2:3]="Aberdeen"
TideStation$Tidestation[TideStation$Tidestation==""&floor(TideStation$Sitecode)==9]="Seattle"
TideStation$Tidestation[TideStation$Tidestation==""&floor(TideStation$Sitecode)==11]="Port Townsend"
TideStation$Tidestation[TideStation$Tidestation==""&TideStation$Sitecode==13.40]="Port Townsend"
#remove TideStation for Sitecodes 15.xx
TideStation=TideStation[floor(TideStation$Sitecode)!=15,]
TideStation[TideStation$Tidestation=="","Sitecode"]

# Add Tidestation value to aerial.pol
aerial.pol$Tidestation=NULL
aerial.pol=merge(aerial.pol,subset(TideStation,select=c("Sitecode","Tidestation")),by="Sitecode",all.x=TRUE)

# add tidestation for sitecodes not in TideStation
aerial.pol$Tidestation[is.na(aerial.pol$Tidestation)&floor(aerial.pol$Sitecode)==9]="Seattle"
aerial.pol$Tidestation[is.na(aerial.pol$Tidestation)&floor(aerial.pol$Sitecode)==11]="Port Townsend"
aerial.pol$Tidestation[is.na(aerial.pol$Tidestation)&floor(aerial.pol$Sitecode)==13]="Port Townsend"


# Bring in old data
old.data <- read.csv("Old Data Import.csv")
# if the first 10 fields are the same, consider this a duplicate record
old.data=old.data[!duplicated(old.data[,1:10]),]
# convert Day to type Date
old.data$Day=as.Date(as.character(old.data$Day),format="%m/%d/%Y")
#create survey time from day and time
old.data$Survey.time=as.POSIXct(paste(as.character(old.data$Day),as.character(old.data$Survey.time)),format="%Y-%m-%d %H:%M:%OS", tz="US/Pacific")
old.data$Tidetime=as.POSIXct(NA)

# fix bad Sitecode values; hopefully these guesses at typos are correct
old.data$Sitecode[old.data$Sitecode=="9:32"]="9.32"
old.data$Sitecode[old.data$Sitecode=="13:18"]="13.18"
old.data$Sitecode[old.data$Sitecode=="11:29"]="11.29"
old.data$Sitecode[old.data$Sitecode=="20.09"]="2.09"
old.data$Sitecode[old.data$Sitecode=="1-Mar"]="3.01"
old.data$Sitecode[old.data$Sitecode=="04.l4"]="4.14"

#remove record with blank Sitecode
old.data=droplevels(old.data[old.data$Sitecode!="",])

# Extract PV
old.data$Species=toupper(as.character(old.data$Species))
old.data=old.data[old.data$Species=="PV",]

# Change sitecode to numeric 
old.data$Sitecode=as.numeric(as.character(old.data$Sitecode))
# remove Sitecode==xx.99 which is in water
old.data=old.data[old.data$Sitecode-floor(old.data$Sitecode)<0.99,]
# remove Sitecode=15.xx which is Oregon
old.data=old.data[floor(old.data$Sitecode)!=15,]

# if count is 0 and estimate is >0 put estimate into count
old.data$Count.total[old.data$Count.total==0]=old.data$Estimated.total[old.data$Count.total==0]
old.data$Count.pups[old.data$Count.pups==0]=old.data$Estimated.pups[old.data$Count.pups==0]
old.data$Count.nonpup=old.data$Count.total-old.data$Count.pups


# make region and stock assignments to old.data
old.data$Stock=NULL
old.data$Region=NULL

# Assign each sitecode to a region to add to the old data
sitetoRegion=unique(aerial.pol[,c("Sitecode","Region")])
sitetoRegion=sitetoRegion[order(sitetoRegion$Sitecode),]

#Create sitetoStock table to use with old.data
sitetoStock=unique(aerial.pol[c("Sitecode","Stock")])
sitetoStock=sitetoStock[order(sitetoStock$Sitecode),]

old.data=merge(old.data,sitetoRegion,by="Sitecode",all.x=TRUE)
old.data=merge(old.data,sitetoStock,by="Sitecode",all.x=TRUE)

#Fill in missing Region/Stock values
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==1]="Columbia River"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==1]="Coastal"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==2]="Willapa Bay"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==2]="Coastal"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==3]="Grays Harbor"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==3]="Coastal"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==4]="Olympic Coast S"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==4]="Coastal"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==5]="Olympic Coast N"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==5]="Coastal"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)%in%6:7]="Strait of Juan de Fuca"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)%in%6:7]="Northern Inland"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==8]="Hood Canal"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==8]="Hood Canal"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==9]="Eastern Bays"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==9]="Northern Inland"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==10]="Puget Sound"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==10]="Southern Puget Sound"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)%in%11:13]="San Juan Islands"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)%in%11:13]="Northern Inland"
old.data$Region[is.na(old.data$Region)&floor(old.data$Sitecode)==14]="BC"
old.data$Stock[is.na(old.data$Stock)&floor(old.data$Sitecode)==14]="CANADA"

any(is.na(old.data$Region))
any(is.na(old.data$Stock))

# merge TideStation value to old.data
old.data$Tidestation=NULL
old.data=merge(old.data,subset(TideStation,select=c("Sitecode","Tidestation")),by="Sitecode",all.x=TRUE)
# add some missing TideStation values
old.data$Tidestation[is.na(old.data$Tidestation)&floor(old.data$Sitecode)==4]="Aberdeen"
old.data$Tidestation[is.na(old.data$Tidestation)&floor(old.data$Sitecode)%in%c(7,9,11,13)]="Port Townsend"
old.data$Tidestation[is.na(old.data$Tidestation)&floor(old.data$Sitecode)==8]="Seattle"
any(is.na(old.data$Tidestation))

# add tidestation for sitecodes not in TideStation
old.data$Tidestation[is.na(old.data$Tidestation)&floor(old.data$Sitecode)==1]="Astoria"
old.data$Tidestation[is.na(old.data$Tidestation)&floor(old.data$Sitecode)%in%2:3]="Aberdeen"

#add year to old.data and aerial.pol
aerial.pol$Year=year(aerial.pol$Day)
old.data$Year=year(old.data$Day)

#check to make sure names in order and match
names(old.data)==names(aerial.pol)

#rbind data sets into a single dataframe
pvdata=rbind(old.data,aerial.pol)

#drop any unused factor levels
pvdata=droplevels(pvdata)

# sum counts and combine when counts are of same sitecode within 50 minutes
pv.df <- combine.surveytime(pvdata, "PV")

#check to make sure totals still match by stock/region and year; have to combine factor variables
#because they are re-ordered in combine.surveytime
pvdata$Stock=factor(as.character(pvdata$Stock))
pv.df$Stock=factor(as.character(pv.df$Stock))
pvdata$Region=factor(as.character(pvdata$Region))
pv.df$Region=factor(as.character(pv.df$Region))
pv.df$year=factor(pv.df$Year,levels=1975:2019)
pvdata$year=factor(pvdata$Year,levels=1975:2019)

with(pv.df,tapply(Count.total,list(Stock,Year),sum,na.rm=T))-with(pvdata,tapply(Count.total,list(Stock,Year),sum,na.rm=T))
with(pv.df,tapply(Count.total,list(Region,Year),sum,na.rm=T))-with(pvdata,tapply(Count.total,list(Region,Year),sum,na.rm=T))

# reduce number of fields
use.fields=c("Sitecode","Day","Tidetime","Survey.time","Species","Count.total","Count.pups","Count.nonpup","Estimated.total","Estimated.pups","Tide.ht..ft.","Region","Stock","Tidestation","Year")
pv.df=subset(pv.df,select=use.fields)
# add julian date for selection
pv.df$Julian <- as.numeric(format(as.Date(pv.df$Day), "%j"))

# some records have stock NA because they are in water but counttype not set to 2; these are dropped; also drop Oregon and CANADA
pv.df <- droplevels(pv.df[!is.na(pv.df$Stock)&pv.df$Region!="OREGON"&pv.df$Stock!="CANADA",])

# order by Survey.time
pv.df=pv.df[order(pv.df$Survey.time),]

# add seq number and select fields to send file to Josh for tide values
pv.df$seq=1:nrow(pv.df)

#create year factor variable with all years from 1975-2019
pv.df$year=factor(pv.df$Year,levels=1975:2019)
pvtot=with(pv.df[pv.df$use,],tapply(Count.total,list(year,Stock),sum))

### This has been commented out and could be deleted. It was only used to construct file to send to Josh
# # create dataframe with all data to get tides from Josh
# pvfortides=subset(pv.df,select=c("seq","Sitecode","Survey.time","Tidestation"))
# 
# TideStation=read.csv("LocationTideStation.csv")
# TideStation=unique(subset(TideStation,select=c("Sitecode","Latitude.DD","Longitude.DD")))
# siteLat=with(TideStation,tapply(Latitude.DD,Sitecode,mean,na.rm=T))
# siteLon=with(TideStation,tapply(Longitude.DD,Sitecode,mean,na.rm=T))
# Location=data.frame(Sitecode=names(siteLat),Latitude=siteLat,Longitude=siteLon)
# Location$Latitude[is.nan(Location$Latitude)]=NA
# Location$Longitude[is.nan(Location$Longitude)]=NA
# Location$Sitecode=as.numeric(as.character(Location$Sitecode))
# 
# # add Lat and Lon where available for sitecode
# pvfortides=merge(pvfortides,Location,by="Sitecode",all.x=TRUE,sort=FALSE)
# 
# #Add NOAA tidestation numbers
# NOAANumbers=data.frame(Tidestation=c("Aberdeen","Astoria","Port Townsend","Seattle"),NOAA=c(9441102,9439040,9444900,9447130))
# pvfortides=merge(pvfortides,NOAANumbers,by="Tidestation",all.x=TRUE,sort=FALSE)
# 
# #order by seq
# pvfortides=pvfortides[order(pvfortides$seq),]
# rownames(pvfortides)=NULL
# 
# # sort columns in desired order
# pvfortides=pvfortides[,c("seq","Sitecode","Survey.time","Latitude","Longitude","Tidestation","NOAA")]
# 
# # output file for Josh to use
# write.csv(pvfortides,"pvfortides.csv",row.names=FALSE)

#read in tide values from Josh
tide_values=as.data.frame(readr::read_csv("Tides from Josh.csv"))
tide_values$survey_time_UTC = tide_values$survey_time
tide_values$time_from_high_minutes=survey_time_UTC = as.numeric((tide_values$survey_time_UTC-tide_values$nearest_high_time)/60)
tide_values$time_from_low_minutes=survey_time_UTC = as.numeric((tide_values$survey_time_UTC-tide_values$nearest_low_time)/60)
#add tide values to pv.df
pv.df=cbind(pv.df,tide_values[,c("nearest_high_height","nearest_low_height","tide_height","time_from_high_minutes","time_from_low_minutes")])

#remove Tide.ht..ft. and Tidetime which were manually input; Values from Josh and manual tide ht values
# agreed 97% of the time within +/- 5%. Manual values were incomplete because it didn't include low tide and high tide times
pv.df$Tide.ht..ft.=NULL
pv.df$Tidetime=NULL
# remove seq field no longer needed after tides added
pv.df$seq=NULL

# write out data as csv
write.csv(pv.df,file="pvdf.csv",row.names=FALSE)

# see data_selection.r for read.csv code