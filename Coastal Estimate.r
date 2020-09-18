# Use cf from Huber et al. 1.53 cv=0.065
cf=1.53
p=1/cf
cv_cf=0.065
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
  data=data[order(paste(data$Year,100*data$Sitecode,".")),]
  # wts are the reciprocal of the number of counts at a site in a year to use the mean at the site
  wts=unlist(sapply(split(data,list(data$Year,100*data$Sitecode)),function(x) rep(1/nrow(x),nrow(x))))
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
# Coastal Estuaries
CEdata=droplevels(pv.df.seldatesyears[pv.df.seldatesyears$Region%in%c("Columbia River", "Grays Harbor","Willapa Bay"),])
CEresults=estimate_abundance(CEdata)
# plot abundance estimates and 95% confidence intervals
pdf(file="CE Abundance.pdf")
plot(sort(unique(CEdata$Year)),CEresults$Abundance,xlab="Year",ylab="Abundance Estimate",type="b",main="Coastal Estuaries",ylim=c(0,25000))
lines(sort(unique(CEdata$Year)),CEresults$LCL,lty=2)
lines(sort(unique(CEdata$Year)),CEresults$UCL,lty=2)
dev.off()
# Output table of results
write.csv(CEresults,file="CEResults.csv",row.names=FALSE)
#Outer Coast
OCdata=droplevels(pv.df.seldatesyears[pv.df.seldatesyears$Region%in%c("Olympic Coast N","Olympic Coast S"),])
OCresults=estimate_abundance(OCdata)
# plot abundance estimates and 95% confidence intervals
pdf(file="OC Abundance.pdf")
plot(sort(unique(OCdata$Year)),OCresults$Abundance,xlab="Year",ylab="Abundance Estimate",type="b",main="Outer Coast",ylim=c(0,10000))
lines(sort(unique(OCdata$Year)),OCresults$LCL,lty=2)
lines(sort(unique(OCdata$Year)),OCresults$UCL,lty=2)
dev.off()
# Output table of results
write.csv(OCresults,file="OCResults.csv",row.names=FALSE)


# Total Coastal-only uses years in which all coastal regions were surveyed
Cdata=droplevels(pv.df.seldatesyears[pv.df.seldatesyears$Stock=="Coastal"&pv.df.seldatesyears$Year%in%CEYears&pv.df.seldatesyears$Year%in%OCYears,])
Cresults=estimate_abundance(Cdata)
# plot abundance estimates and 95% confidence intervals
pdf(file="Coastal Abundance.pdf")
plot(sort(unique(Cdata$Year)),Cresults$Abundance,xlab="Year",ylab="Abundance Estimate",type="b",main="Coastal Stock",ylim=c(0,35000))
lines(sort(unique(Cdata$Year)),Cresults$LCL,lty=2)
lines(sort(unique(Cdata$Year)),Cresults$UCL,lty=2)
dev.off()
# Output table of results
write.csv(Cresults,file="CResults.csv",row.names=FALSE)

# Here is a check on the estimate_abundance code
annual_counts=colSums(with(Cdata,tapply(Count.total,list(Sitecode,Year),mean,na.rm=T)),na.rm=T)
annual_abundance=annual_counts*cf
# the following is a gross check which will underestimate std error
# because it uses binomial variance on total rather than individual counts 
stderror_annual_abundance=sqrt((cv_cf*annual_abundance)^2+annual_counts*p*(1-p))
