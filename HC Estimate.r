library(lubridate)
# Hood Canal data analysis
HCdata=droplevels(pv.df.seldatesyears[pv.df.seldatesyears$Stock=="Hood Canal"&pv.df.seldatesyears$Sitecode!=8&pv.df.seldatesyears$Year>=1991,])
# add hi,lo,other factor variable for sites 
HCdata$tidetype=rep("Other",nrow(HCdata))
HCdata$tidetype[HCdata$Sitecode%in%c(8.04,8.05,8.06,8.08,8.13,8.14,8.15)]="High"
HCdata$tidetype[HCdata$Sitecode%in%c(8.16,8.19)]="Low"
HCdata$tidetype=factor(HCdata$tidetype)
# this is what was used in London et al although not strictly correct since 227 in non leap and 228 in leap years
HCdata$MFAug15 <- (HCdata$Julian- 226)/31
HCdata$hour = factor(hour(HCdata$Survey.time),levels=0:23)
# temporary value until I get tides but will be used for sites other than Hi tide sites
HCdata$MinFHi=0
HCdata$MinFHi[HCdata$tidetype!="High"]=65
# Create categories defined in London et al
HCdata$MinFHi.Cat=cut(HCdata$MinFHi,seq(-448-16,482+16,32),labels=seq(-448,482,32))

# Load results from London et al to create survey specific correction factors
load("all.fit.1.rda")
# get fixed effect table
fe=all.fit.1$fixed.effects
# create design matrix for survey data and add weighting of 1/3 for each of three years
dm=cbind(rep(1,nrow(HCdata)),model.matrix(~-1+hour,data=HCdata),model.matrix(~-1+MinFHi.Cat,data=HCdata),
         model.matrix(~-1+MFAug15,data=HCdata),model.matrix(~-1+hour:MFAug15,data=HCdata))
dm=cbind(dm[,1:56],matrix(1/3,nrow=nrow(dm),ncol=3),dm[,57:ncol(dm)])
#estimate of expected proportions hauled out at the covariate values when counts occurred
p=as.vector(plogis(dm%*%fe$estimate))
#variance-covariance matrix of proportion estimates
vc_p=(dm[,!is.na(fe$std.err)]*p*(1-p))%*%all.fit.1$covb%*%t(dm[,!is.na(fe$std.err)]*p*(1-p))
#multiplicative correction factor for counts
cf=1/p
#variance-covariance matrix of correction factors
vc_cf=vc_p*cf^4
# Abundance estimate and 
HCdata$AbundanceEstimate=cf*HCdata$Count.total
# Variance-covariance matrix of abundance estimates; assumes binomial for count on diagonal - no covariance on counts
vc_abundance=HCdata$Count.total%*%t(HCdata$Count.total)*vc_cf+diag(HCdata$AbundanceEstimate*p*(1-p))

# Annual abundance estimates
HCdata=HCdata[order(HCdata$Year,HCdata$Sitecode),]
wts=unlist(sapply(split(HCdata,list(HCdata$Year,formatC(HCdata$Sitecode,digits=3,flag="#"))),function(x) rep(1/nrow(x),nrow(x))))
HCdata$wts=wts[order(names(wts))]

X=sapply(sort(unique(HCdata$Year)),function(x){
  Weights=HCdata$wts
  Weights[!HCdata$Year==x]=0
  return(Weights)
})

AnnualEstimates=HCdata$AbundanceEstimate%*%X
var_AnnualEstimates=t(X)%*%vc_abundance%*%X

c=exp(1.96*sqrt(log(1+diag(var_AnnualEstimates)/AnnualEstimates^2)))

plot(sort(unique(HCdata$Year)),AnnualEstimates,xlab="Year",ylab="Abundance Estimate",type="b",ylim=c(0,6000))
lines(sort(unique(HCdata$Year)),AnnualEstimates/c,lty=2)
lines(sort(unique(HCdata$Year)),AnnualEstimates*c,lty=2)

library(glmmTMB)
mod= glmmTMB(Count.total~Year+MFAug15+(1|Sitecode),zi=~1,family=poisson,data=HCdata)
summary(mod)
