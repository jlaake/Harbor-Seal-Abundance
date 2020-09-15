# Use cf from Huber et al. 1.53 cv=0.065
cf=1.53
p=1/cf
cv_cf=0.065

# 
# Plowing ahead. I have set stock for 10.0 to be SPS and remove 10.89 because these are in tow and not clearly from any known stock
#
mod_pvdf=pv.df.seldatesyears
mod_pvdf$Stock[mod_pvdf$Sitecode==10]="Southern Puget Sound"
mod_pvdf=mod_pvdf[!mod_pvdf$Sitecode==10.89,]

# create function to compute abundance estimates and v-c matrix
estimate_abundance=function(data)
{
  #variance-covariance matrix of correction factors
  vc_cf=matrix((cv_cf*cf)^2,nrow=nrow(data),ncol=nrow(data))
  # Abundance estimate 
  data$AbundanceEstimate=cf*data$Count.total
  # Variance-covariance matrix of abundance estimates; assumes binomial for count on diagonal - no covariance on counts
  vc_abundance=data$Count.total%*%t(data$Count.total)*vc_cf+diag(data$AbundanceEstimate*p*(1-p))
  # Annual abundance estimates
  data=data[order(data$Year,data$Sitecode),]
  # wts are the reciprocal of the number of counts at a site in a year to use the mean at the site
  wts=unlist(sapply(split(data,list(data$Year,formatC(data$Sitecode,digits=3,flag="#"))),function(x) rep(1/nrow(x),nrow(x))))
  data$wts=wts[order(names(wts))]
  # X is a matrix of weights to construct the annual estimates
  X=sapply(sort(unique(data$Year)),function(x){
    Weights=data$wts
    Weights[!data$Year==x]=0
    return(Weights)
  })
  # Compute annual count using means when site is replicated
  AnnualCount=as.vector(data$Count.total%*%X)
  # Compute AnnualEstimates and their variance-covariance matrix; they co-vary because they are using the same London et al model
  AnnualEstimates=as.vector(data$AbundanceEstimate%*%X)
  names(AnnualEstimates)=sort(unique(data$year))
  var_AnnualEstimates=t(X)%*%vc_abundance%*%X
  # for 95% confidence interval based on log normal distribution
  SE=sqrt(diag(var_AnnualEstimates))
  CV=SE/AnnualEstimates
  c=exp(1.96*sqrt(log(1+CV^2)))
  UCL=AnnualEstimates*c
  LCL=AnnualEstimates/c
  # Nmin calculation uses z=0.842
  Nmin=AnnualEstimates/exp(.842*sqrt(log(1+CV^2)))
  df=data.frame(Year=sort(unique(data$Year)),Count=round(AnnualCount),Abundance=round(AnnualEstimates),Stderror=round(SE,1),CV=round(CV,3),LCL=round(LCL),UCL=round(UCL),Nmin=round(Nmin))  
  rownames(df)=NULL
  return(df)
}

#Northern Inland-only uses years in which SJI,SJF and EB were all surveyed
NIdata=droplevels(mod_pvdf[mod_pvdf$Stock=="Northern Inland"&mod_pvdf$Year%in%EBYears&mod_pvdf$Year%in%SJFYears&mod_pvdf$Year%in%SJIYears,])
NIresults=estimate_abundance(NIdata)
# plot abundance estimates and 95% confidence intervals
pdf(file="Northern Inland Abundance.pdf")
plot(sort(unique(NIdata$Year)),NIresults$Abundance,xlab="Year",ylab="Abundance Estimate",type="b",ylim=c(0,12000),main="Northern Inland")
lines(sort(unique(NIdata$Year)),NIresults$LCL,lty=2)
lines(sort(unique(NIdata$Year)),NIresults$UCL,lty=2)
dev.off()
# Output table of results
write.csv(NIresults,file="NIResults.csv",row.names=FALSE)

#Southern Puget Sound
SPSdata=droplevels(mod_pvdf[mod_pvdf$Stock=="Southern Puget Sound",])
SPSresults=estimate_abundance(SPSdata)
# plot abundance estimates and 95% confidence intervals
pdf(file="Southern Puget Sound Abundance.pdf")
plot(sort(unique(SPSdata$Year)),SPSresults$Abundance,xlab="Year",ylab="Abundance Estimate",type="b",ylim=c(0,1000),main="Southern Puget Sound")
lines(sort(unique(SPSdata$Year)),SPSresults$LCL,lty=2)
lines(sort(unique(SPSdata$Year)),SPSresults$UCL,lty=2)
dev.off()
# Output table of results
write.csv(SPSresults,file="SPSResults.csv",row.names=FALSE)

