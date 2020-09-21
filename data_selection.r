#clean workspace
rm(list=ls())
#Selection of years and dates for surveys
pv.df=read.csv(file="pvdf.csv")

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

# Years to use by region
HCYears=c(1991,1992,1993,1996,1998,1999,2000,2002,2003,2004,2005,2010,2013,2019)
SPSYears=c(1985,1991,1992,1993,1994,1996,1998,2000,2004,2005,2008,2010,2013,2014,2019)
EBYears=c(1983,1984,1985,1986,1987,1988,1989,1991,1992,1993,1994,1995,1996,1998,1999,2000,2001,2002,2004,2005,2006,2007,2008,2010,2013,2014,2019)
SJIYears=c(1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1998,1999,2000,2001,2002,2004,2005,2006,2010,2013,2014,2019)
SJFYears=c(1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1998,1999,2000,2004,2013,2014,2019)
CEYears=c(1975:1978,1980:1989,1991:1997,1999:2001,2004:2005,2007,2014)
OCYears=c(1980:1981,1983,1986:1987,1989,1991:1997,1999:2001,2004:2005,2007,2014)

# restrict to dates and years used
pv.df.seldatesyears=droplevels(pv.df.seldates[(pv.df.seldates$Region=="Hood Canal" & pv.df.seldates$Year%in%HCYears) |  (pv.df.seldates$Region=="San Juan Islands" & pv.df.seldates$Year%in%SJIYears) | 
                                                (pv.df.seldates$Region=="Strait of Juan de Fuca" & pv.df.seldates$Year%in%SJFYears) | (pv.df.seldates$Region=="Eastern Bays" & pv.df.seldates$Year%in%EBYears) |
                                                (pv.df.seldates$Region=="Puget Sound" & pv.df.seldates$Year%in%SPSYears)|
                                                (pv.df.seldates$Region%in%c("Columbia River", "Grays Harbor","Willapa Bay")& pv.df.seldates$Year%in%CEYears)|
                                                (pv.df.seldates$Region%in%c("Olympic Coast N","Olympic Coast S")& pv.df.seldates$Year%in%OCYears)
                                              ,])



