library(ggplot2)
library(lubridate)
library(reshape2)
library(dplyr)

#############
##This code adds polygon#, Region, and Stock (if applicable) to each record
############
#dfa: dataframe you want polygon#s added
#dfb: dataframe that has which sitecode belongs to which polygon

sitecode.polygon <- function(dfa, dfb){
	y <- rownames(dfa)
	df.1 <- dfa[0,]
	df.1$Polygon <- factor()
	df.1$Region <- factor()
	df.1$Stock <- factor()
	for(i in y){
		q <- dfa[i,]
		qa <- q$Sitecode
		if(qa %in% dfb$Sitecode){
			t <- dfb[dfb$Sitecode==qa,]
			q$Polygon <- unique(t$Polygon.ID)
			q$Region <- unique(t$Region)
			if(q$Species == "PV"){
				q$Stock <- unique(t$Stock)
				}
			else{q$Stock <- "NA"}	
			}	
		else{q$Polygon <- "MISSING"; q$Region <- "MISSING"; q$Stock <- "MISSING"}
		df.1 <- rbind(df.1,q)
		}
	df.1
	}
#jig <- droplevels(aerial.df[150:300,])
#jiggles <- sitecode.polygon(jig, location.df)
##########################################################################################3
###########Note: the below obs.one code is now sort of redundant. It has been replaced with
###########combine.survey to better represent the target data we're looking for
##########################################################################################
###Let's make a function that reshuffles observations so there's only one
###For right now-using the sitecode as the "multiple runs" instance instead
#of polygon

#dfa: dataframe info coming from (probably aerial.pol or something)
#dieArt: type of observation selection, either random, 1st, or mean
#spc: to temporarily avoid catastrophic implosion of too many
#for/if loops, let's specify a species
#NOTE: this only does random/1st/mean of counts.
#obs.one <- function(dfa, dieArt, spc){
#	df.1 <- dfa[0,]
#	dfb <- droplevels(dfa[dfa$Species==spc,])
#	y <- unique(dfb$Day)
#	for(i in y){
#		q.day <- dfb[dfb$Day==i,]
#		q.day <- droplevels(q.day)
#		day.table <- table(q.day$Sitecode)
#		day.df <- data.frame(day.table)
#		multi <- day.df[day.df$Freq > 1,]
#		sing <- day.df[day.df$Freq < 2,]
#		single.df <- q.day[q.day$Sitecode %in% sing$Var1,]
#		df.1 <- rbind(df.1,single.df)
#		multi.sites <- multi$Var1
#		for(i in multi.sites){
#			q.df <- q.day[q.day$Sitecode ==i,]
#			q.df.1 <- q.df[1,]
#			if(dieArt=="random"){
#				rowname.random <- sample(rownames(q.df),1)
#				random.row <- q.df[rowname.random,]
#				q.df.1 <- random.row
#				}
#			if(dieArt=="first"){
#				rowname.first <- min(rownames(q.df))
#				first.row <- q.df[rowname.first,]
#				q.df.1 <- first.row
#				}
#			if(dieArt=="avg"){
#				count.tot <- mean(q.df$Count.total)
#				count.nonpup <- mean(q.df$Count.nonpup)
#				count.pup <- mean(q.df$Count.pups)
#				q.df.1$Count.total <- count.tot
#				q.df.1$Count.nonpup <- count.nonpup
#				q.df.1$Count.pups <- count.pup
#				}
#			df.1 <- rbind(df.1, q.df.1)
#			}
#		}
#	df.1
#	}
#TIKI <- obs.one(aer.test, "random", "PV")

#alright lets test this monstrosity
#aer.test <- droplevels(aerial.pol[1:200,])
#write.csv(TIKI, "Talc.csv")

#Let's make a function that pulls survey dates
#This is specifically for inland surveys
#dfa: dataframe with all relevant information
#datelist: list of survey dates
library(lubridate)
abundance.survey <- function(dfa, datelist, hood.dates){
		nonhood <- c("Strait of Juan de Fuca", "San Juan Islands",
				"Eastern Bays", "Puget Sound")
		nonhood.df <- droplevels(dfa[dfa$Region %in% nonhood & dfa$Day %in% datelist,])
		if(class(hood.dates)!="Date"){
			survey.df <- nonhood.df
			}
		else{		
			hood.df <- droplevels(dfa[dfa$Region=="Hood Canal" & dfa$Day %in% hood.dates,])
			survey.df <- rbind(nonhood.df, hood.df)
			}
		survey.df
		}
#y.2000 <- c(seq(as.Date("2000-7-31"), as.Date("2000-08-03"), by="days"), seq(as.Date("2000-8-11"), as.Date("2000-08-16"), by="days"))
#hood.2000 <- seq(as.Date("2000-10-2"), as.Date("2000-10-04"), by="days")
#abundance.survey(PVrandom.aerial, y.2000, NA)			

#For future coding, let's see if putting those dates in a separate CSV
#file is easier. It's probably way faster. Can adjust code later. Let's 
#work with what we got for now.

#Similar as above, but targeted for picking one survey per observation
#period. This code is intended for use with the dataframe that already
#has these periods extracted-i.e. the "abundance.survey" has already been
#run.

#dfa: dataframe info coming from (probably aerial.pol or something)
#dieArt: type of observation selection, either random, 1st, or mean

obs.one2 <- function(dfa, dieArt){
	df.1 <- dfa[0,]
	y <- unique(dfa$Year)
	for(i in y){
		q.year <- dfa[dfa$Year==i,]
		q.year <- droplevels(q.year)
		year.table <- table(q.year$Sitecode)
		year.df <- data.frame(year.table)
		multi <- year.df[year.df$Freq > 1,]
		sing <- year.df[year.df$Freq < 2,]
		single.df <- q.year[q.year$Sitecode %in% sing$Var1,]
		df.1 <- rbind(df.1,single.df)
		multi.sites <- multi$Var1
		for(i in multi.sites){
			q.df <- q.year[q.year$Sitecode ==i,]
			q.df.1 <- q.df[1,]
			if(dieArt=="random"){
				rowname.random <- sample(rownames(q.df),1)
				random.row <- q.df[rowname.random,]
				q.df.1 <- random.row
				}
			if(dieArt=="first"){
				rowname.first <- min(rownames(q.df))
				first.row <- q.df[rowname.first,]
				q.df.1 <- first.row
				}
			if(dieArt=="avg"){
				count.tot <- mean(q.df$Count.total)
				count.nonpup <- mean(q.df$Count.nonpup)
				count.pup <- mean(q.df$Count.pups)
				q.df.1$Count.total <- count.tot
				q.df.1$Count.nonpup <- count.nonpup
				q.df.1$Count.pups <- count.pup
				}
			df.1 <- rbind(df.1, q.df.1)
			}
		}
	df.1
	}
#pv.random <- obs.one2(pv.zomer, "random")


`%notin%` <- Negate(`%in%`)

####When in doubt, make a code for it.
############################################
####Combines counts for sites where there are more than 
####one observation per day. Seperated by 50 minutes
#######################################################
combine.surveytime <- function(dfa, species){
		df.1 <- dfa[0,]
		df.1$Survey.time <- ymd_hms(df.1$Survey.time, tz="US/Pacific")
		dfb <- droplevels(dfa[dfa$Species == species,])
		y <- unique(dfb$Day)
		for(i in y){
			q.day <- dfb[dfb$Day==i,]
			q.day <- droplevels(q.day)
			day.table <- table(q.day$Sitecode)
			day.df <- data.frame(day.table)
			multi <- day.df[day.df$Freq > 1,]
			sing <- day.df[day.df$Freq < 2,]
			single.df <- q.day[q.day$Sitecode %in% sing$Var1,]
			df.1 <- rbind(df.1,single.df)
			multi.sites <- multi$Var1
			for(i in multi.sites){
				q.df <- q.day[q.day$Sitecode ==i,]
				q.df$Survey.time <- ymd_hms(q.df$Survey.time, tz="US/Pacific")
				min.time <- min(q.df$Survey.time)
				time.window <- min.time + dminutes(50)
				multi.time <- droplevels(q.df[q.df$Survey.time <= time.window,])
				q.df.1 <- multi.time[1,]
					q.df.1$Count.total <- sum(multi.time$Count.total)
					q.df.1$Count.nonpup <- sum(multi.time$Count.nonpup)
					q.df.1$Count.pups <- sum(multi.time$Count.pups)
					q.df.1$Estimated.total <- sum(multi.time$Estimated.total)
					q.df.1$Estimated.pups <- sum(multi.time$Estimated.pups)
					df.1 <- rbind(df.1, q.df.1)
				if(any(q.df$Survey.time > time.window)){	
						sec.survey <- droplevels(q.df[q.df$Survey.time > time.window,])
						q.df.2 <- sec.survey[1,]
						q.df.2 <- sec.survey[1,]
						q.df.2$Count.total <- sum(sec.survey$Count.total)
						q.df.2$Count.nonpup <- sum(sec.survey$Count.nonpup)
						q.df.2$Count.pups <- sum(sec.survey$Count.pups)
						q.df.2$Estimated.total <- sum(sec.survey$Estimated.total)
						q.df.2$Estimated.pups <- sum(sec.survey$Estimated.pups)
						df.1 <- rbind(df.1, q.df.2)
						}
				}
			}
		df.1
		}
#jiggles <- combine.surveytime(jig, "PV")

#jig <- read.csv("test.csv")
#jig$Day <- as.Date(jig$Day, "%d-%b-%y")
#jig$Survey.time <- as.POSIXct(jig$Survey.time,format="%I:%M:%S %p", tz="US/Pacific")

#write.csv(jiggles, "Talc.csv")


#jig$Survey.time <- ymd_hms(jig$Survey.time, tz="US/Pacific")

##############################################
###Time to make even more code.
###This time to narrow down the tide survey times. Pick ones within 2.5 hours of low
###tide on each end of the spectrum. Exclude those outside that window. Include
###those outside the window if it's the only survey for that day.
################################################

lowtide.df <- function(dfa){
		df.1 <- dfa[0,]
		df.1$Tidetime <- ymd_hms(df.1$Survey.time, tz="US/Pacific")
		missing.tides <- droplevels(dfa[is.na(dfa$Tidetime),])
		dfaB <- droplevels(dfa[!is.na(dfa$Tidetime),])
		y <- unique(dfaB$Day)
		for(i in y){
			q.day <- droplevels(dfaB[dfaB$Day==i,])
			day.df <- data.frame(table(q.day$Sitecode))
			multi <- day.df[day.df$Freq > 1,]
			sing <- day.df[day.df$Freq < 2,]
			single.df <- q.day[q.day$Sitecode %in% sing$Var1,]
			df.1 <- rbind(df.1,single.df)
			multi.sites <- multi$Var1
			q.dayB <- droplevels(q.day[!is.na(q.day$Tidetime),])
			for(i in multi.sites){
				q.df <- q.dayB[q.dayB$Sitecode ==i,]
				q.df$Tidetime <- ymd_hms(q.df$Tidetime, tz="US/Pacific")
				q.df$Survey.time <- ymd_hms(q.df$Survey.time, tz="US/Pacific")
				bf.lowtide <- min(q.df$Tidetime) - dminutes(150)
				af.lowtide <- min(q.df$Tidetime) + dminutes(150)
				multi.time <- droplevels(q.df[q.df$Survey.time <= af.lowtide & q.df$Survey.time >= bf.lowtide,])
				if(nrow(multi.time) == 0){
					rowname.random <- sample(rownames(q.df),1)
					q.df.1 <- q.df[rowname.random,]
					df.1 <- rbind(df.1, q.df.1)
					}
				else{
					df.1 <- rbind(df.1, multi.time)
					}
				}
			}
			df.1 <- rbind(df.1, missing.tides)
			df.1
			}
#test <- lowtide.df(pv.df)
#write.csv(test, "Talc.csv")
				
###################################
######Purely to try this abundance occupancy stuff
###################################
#nvm looks like there aren't multiple visits really

occ.survey <- function(dfa, datelist1, datelist2, hood.dates1, hood.dates2){
		nonhood <- c("Strait of Juan de Fuca", "San Juan Islands",
				"Eastern Bays", "Puget Sound")
		nonhood.df1 <- droplevels(dfa[dfa$Region %in% nonhood & dfa$Date %in% datelist1,])
		nonhood.df1$Survey <- 1
		hood.df1 <- droplevels(dfa[dfa$Region=="Hood Canal" & dfa$Date %in% hood.dates1,])
		hood.df1$Survey <- 1
			if(datelist2 != "NA"){
				nonhood.df2 <- droplevels(dfa[dfa$Region %in% nonhood & dfa$Date %in% datelist2,])
				nonhood.df1$Survey <- 2
				}
			if(hood.dates2 != "NA"){
				hood.df2 <- droplevels(dfa[dfa$Region=="Hood Canal" & dfa$Date %in% hood.dates2,])
				hood.df2$Survey <- 2
				}
		
		survey.df <- rbind(nonhood.df, hood.df)
		survey.df
		}




