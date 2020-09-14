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
CEYears=c(1975:1978,1980:1989,1991:1997,1999:2001,2004:2005,2007,2014)
# Coastal Estuaries
CEdata=droplevels(pv.df[pv.df$Region%in%c("Columbia River", "Grays Harbor","Willapa Bay")&pv.df$Year%in%CEYears,])
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
OCYears=c(1980:1981,1983,1986:1987,1989,1991:1997,1999:2001,2004:2005,2007,2014)
OCdata=droplevels(pv.df[pv.df$Region%in%c("Olympic Coast N","Olympic Coast S")&pv.df$Year%in%OCYears,])
OCresults=estimate_abundance(OCdata)
# plot abundance estimates and 95% confidence intervals
pdf(file="OC Abundance.pdf")
plot(sort(unique(OCdata$Year)),OCresults$Abundance,xlab="Year",ylab="Abundance Estimate",type="b",main="Outer Coast",ylim=c(0,10000))
lines(sort(unique(OCdata$Year)),OCresults$LCL,lty=2)
lines(sort(unique(OCdata$Year)),OCresults$UCL,lty=2)
dev.off()
# Output table of results
write.csv(OCresults,file="OCResults.csv",row.names=FALSE)
