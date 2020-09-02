rm(list=ls())
library(readr)
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

#Note: the below POSIX code adds a date to the time stamp, it's going to be whatever date it
#is when you run the code. Date won't interfere with any code below. Needs to be run for the 
#code below in order to do things with time.
#aerial.pol$Survey.time <- as.POSIXct(aerial.pol$Survey.time,format="%I:%M:%S %p", tz="US/Pacific")
#aerial.pol$Tidetime <- as.POSIXct(aerial.pol$Tidetime,format="%I:%M:%S %p", tz="US/Pacific")
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

#assign use = TRUE if in date range and FALSE otherwise
nonhood.dates <- 198:245
hood.dates <- 234:314
coastal.dates <- 121:181
nonhood <- c("Northern Inland", "Southern Puget Sound")

#Piece out target stocks by target dates
pv.df$use=FALSE
pv.df$use[pv.df$Stock %in% nonhood & pv.df$Julian %in% nonhood.dates]=TRUE
pv.df$use[pv.df$Stock=="Hood Canal" & pv.df$Julian %in% hood.dates]=TRUE
pv.df$use[pv.df$Stock=="Coastal" & pv.df$Julian %in% coastal.dates]=TRUE

#create year factor variable with all years from 1975-2019
pv.df$year=factor(pv.df$Year,levels=1975:2019)
pvtot=with(pv.df[pv.df$use,],tapply(Count.total,list(year,Stock),sum))


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

pv.df=cbind(pv.df,tide_values[,c("nearest_high_height","nearest_low_height","tide_height","time_from_high_minutes","time_from_low_minutes")])

#remove Tide.ht..ft. and Tidetime which were manually input; Values from Josh and manual tide ht values
# agreed 97% of the time within +/- 5%. Manual values were incomplete because it didn't include low tide and high tide times
pv.df$Tide.ht..ft.=NULL
pv.df$Tidetime=NULL
# remove seq field no longer needed after tides added
pv.df$seq=NULL

#table total counts by year and stock
with(pv.df[pv.df$use,],tapply(Count.total,list(year,Stock),sum))

# From here forward only use those with use==TRUE in .seldates
pv.df.seldates=pv.df[pv.df$use,]


#total counts by year and region for inland waters
pvyeartable=with(pv.df.seldates,tapply(Count.total,list(year,Region),sum))[,1:5]
pvyeartable[is.na(pvyeartable)]=0

#read in Steve's yes/no table
yesno=read.csv("SteveYesNoTable.csv")
yesno=apply(yesno[,2:6],2,function(x) as.numeric(x=="yes"))

pvyeartable
pvyeartable*yesno

HCBysiteyear=with(pv.df.seldates[pv.df.seldates$Stock=="Hood Canal",],tapply(Count.total,list(year,Sitecode),sum))
write.csv(HCBysiteyear,"HCbysiteyear.csv")

SPSBysiteyear=with(pv.df.seldates[pv.df.seldates$Stock=="Southern Puget Sound",],tapply(Count.total,list(year,Sitecode),sum))
write.csv(SPSBysiteyear,"SPSbysiteyear.csv")

SJIBysiteyear=with(pv.df.seldates[pv.df.seldates$Region=="San Juan Islands",],tapply(Count.total,list(year,Sitecode),sum))
write.csv(SJIBysiteyear,"SJIbysiteyear.csv")

SJFBysiteyear=with(pv.df.seldates[pv.df.seldates$Region=="Strait of Juan de Fuca",],tapply(Count.total,list(year,Sitecode),sum))
write.csv(SJFBysiteyear,"SJFbysiteyear.csv")

EBBysiteyear=with(pv.df.seldates[pv.df.seldates$Region=="Eastern Bays",],tapply(Count.total,list(year,Sitecode),sum))
write.csv(EBBysiteyear,"EBbysiteyear.csv")


# Years to use by region
HCYears=c(1991,1992,1993,1996,1998,1999,2000,2002,2003,2004,2005,2010,2013,2019)
SPSYears=c(1985,1991,1992,1993,1994,1996,1998,2000,2004,2005,2008,2010,2013,2014,2019)
EBYears=c(1983,1984,1985,1986,1987,1988,1989,1991,1992,1993,1994,1995,1996,1998,1999,2000,2001,2002,2004,2005,2006,2007,2008,2010,2013,2014,2019)
SJIYears=c(1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1998,1999,2000,2001,2002,2004,2005,2006,2010,2013,2014,2019)
SJFYears=c(1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1998,1999,2000,2004,2013,2014,2019)

#NIyears=unique(c(EBYears,SJIYears,SJFYears))[unique(c(EBYears,SJIYears,SJFYears))%in%EBYears & unique(c(EBYears,SJIYears,SJFYears))%in%SJFYears &unique(c(EBYears,SJIYears,SJFYears))%in%SJIYears]
#NI=pv.df.seldates[pv.df.seldates$Stock=="Northern Inland" & pv.df.seldates$Year%in%NIyears,]
#pv.mean <- obs.one2(NI, "avg")

# restrict to dates and years used
pv.df.seldatesyears=droplevels(pv.df.seldates[(pv.df.seldates$Region=="Hood Canal" & pv.df.seldates$Year%in%HCYears) |  (pv.df.seldates$Region=="San Juan Islands" & pv.df.seldates$Year%in%SJIYears) | 
              (pv.df.seldates$Region=="Strait of Juan de Fuca" & pv.df.seldates$Year%in%SJFYears) | (pv.df.seldates$Region=="Eastern Bays" & pv.df.seldates$Year%in%EBYears) |
              (pv.df.seldates$Region=="Puget Sound" & pv.df.seldates$Year%in%SPSYears),])

# restrict to years used but not dates
pv.df.selyears=droplevels(pv.df[(pv.df$Region=="Hood Canal" & pv.df$Year%in%HCYears) |  (pv.df$Region=="San Juan Islands" & pv.df$Year%in%SJIYears) | 
                                                (pv.df$Region=="Strait of Juan de Fuca" & pv.df$Year%in%SJFYears) | (pv.df$Region=="Eastern Bays" & pv.df$Year%in%EBYears) |
                                                (pv.df$Region=="Puget Sound" & pv.df$Year%in%SPSYears),])



# #Code below chooses one observation per site within each target window if there are 
# #multiple observations in a year.
# #"Random" is a random choice.
# #"First" is the first observation made that year.
# #"Mean" is the average of all observations made that year.
# #Output slims down the dataset from multiple observations per site in a year to a single 
# #observation per site per year within target survey windows.
# 
# pv.random <- obs.one2(pv.df.seldatesyears, "random")
# pv.first <- obs.one2(pv.df.seldatesyears, "first")
# pv.mean <- obs.one2(pv.df.seldatesyears, "avg")
# 
# 
# 
# par(mfrow=c(3,2))
# plot_count(pv.mean,"Hood Canal")
# plot_count(pv.mean,"Strait of Juan de Fuca")
# plot_count(pv.mean,"San Juan Islands")
# plot_count(pv.mean,"Puget Sound")
# plot_count(pv.mean,"Eastern Bays")
# 
# 
# #############################################################
# ##Code below sums total counts, nonpup counts, and pup counts 
# ##for each output above.
# ##############################################################
# 
# #"Random" Obs Counts
# PVrandom.tots <- tapply(pv.random$Count.total, list(pv.random$Year, pv.random$Stock), sum, na.rm=TRUE)
# PVrandom.tots
# PVrandom.pups <- tapply(pv.random$Count.pups, list(pv.random$Year, pv.random$Stock), sum, na.rm=TRUE)
# PVrandom.pups
# PVrandom.nonpups <- tapply(pv.random$Count.nonpup, list(pv.random$Year, pv.random$Stock), sum, na.rm=TRUE)
# PVrandom.nonpups
# 
# #"Mean" Obs Counts
# PVmean.tots <- tapply(pv.mean$Count.total, list(pv.mean$Year, pv.mean$Stock), sum, na.rm=TRUE)
# PVmean.tots
# PVmean.pups <- tapply(pv.mean$Count.pups, list(pv.mean$Year, pv.mean$Stock), sum, na.rm=TRUE)
# PVmean.pups
# PVmean.nonpups <- tapply(pv.mean$Count.nonpup, list(pv.mean$Year, pv.mean$Stock), sum, na.rm=TRUE)
# PVmean.nonpups
# 
# #"First" Obs Counts
# PVfirst.tots <- tapply(pv.first$Count.total, list(pv.first$Year, pv.first$Stock), sum, na.rm=TRUE)
# PVfirst.tots
# PVfirst.pups <- tapply(pv.first$Count.pups, list(pv.first$Year, pv.first$Stock), sum, na.rm=TRUE)
# PVfirst.pups
# PVfirst.nonpups <- tapply(pv.first$Count.nonpup, list(pv.first$Year, pv.first$Stock), sum, na.rm=TRUE)
# PVfirst.nonpups
# 
# ######Putting it all together and into a CSV to get a better look.
# PVrandom.tots <- data.frame(PVrandom.tots);PVrandom.tots$Survey <- rownames(PVrandom.tots);PVrandom.tots$SampleType <- "Random";PVrandom.tots$CountType <- "Total"
# PVrandom.pups <- data.frame(PVrandom.pups);PVrandom.pups$Survey <- rownames(PVrandom.pups);PVrandom.pups$SampleType <- "Random";PVrandom.pups$CountType <- "Pups"
# PVrandom.nonpups <- data.frame(PVrandom.nonpups);PVrandom.nonpups$Survey <- rownames(PVrandom.nonpups);PVrandom.nonpups$SampleType <- "Random";PVrandom.nonpups$CountType <- "NonPups"
# 
# PVfirst.tots <- data.frame(PVfirst.tots);PVfirst.tots$Survey <- rownames(PVfirst.tots);PVfirst.tots$SampleType <- "First";PVfirst.tots$CountType <- "Total"
# PVfirst.pups <- data.frame(PVfirst.pups);PVfirst.pups$Survey <- rownames(PVfirst.pups);PVfirst.pups$SampleType <- "First";PVfirst.pups$CountType <- "Pups"
# PVfirst.nonpups <- data.frame(PVfirst.nonpups);PVfirst.nonpups$Survey <- rownames(PVfirst.nonpups);PVfirst.nonpups$SampleType <- "First";PVfirst.nonpups$CountType <- "NonPups"
# 
# PVmean.tots <- data.frame(PVmean.tots);PVmean.tots$Survey <- rownames(PVmean.tots);PVmean.tots$SampleType <- "Mean";PVmean.tots$CountType <- "Total"
# PVmean.pups <- data.frame(PVmean.pups);PVmean.pups$Survey <- rownames(PVmean.pups);PVmean.pups$SampleType <- "Mean";PVmean.pups$CountType <- "Pups"
# PVmean.nonpups <- data.frame(PVmean.nonpups);PVmean.nonpups$Survey <- rownames(PVmean.nonpups);PVmean.nonpups$SampleType <- "Mean";PVmean.nonpups$CountType <- "NonPups"
# 
# 
# PV.counts <- rbind(PVrandom.tots, PVrandom.pups, PVrandom.nonpups, PVfirst.tots, PVfirst.pups, PVfirst.nonpups, PVmean.tots, PVmean.pups, PVmean.nonpups)
# write.csv(PV.counts, "PVCountsStock.csv")
# 
# 
