rm(list=ls())
cf=1.53
cvcf=0.065

## Functions to fit genrealized logistic otherwise known as Pella-Tomlinson function
## originally written by Jeff Breiwick and modified by J. Laake for this specific analysis
project.PT <-function (z,Rm,n0,K,times)
{
  n=vector(mode="numeric",length=max(times))
  n[1]  = n0
  for(i in 2:max(times))
      n[i] = n[i-1]*(1+Rm*(1-(n[i-1]/(K))^z))
  return(n)
}

nls.PT.mod.pe <-function (z,Rm,n0,K,y.obs,time.index,region,rmindex)
{
  counts_byregion=split(y.obs,region)
  times_byregion=split(time.index,region)
  residuals=NULL
  rmi=sapply(split(rmindex,region),unique)
  for(i in 1:length(counts_byregion))
  {
    y.obs=counts_byregion[[i]]
    times=times_byregion[[i]]
    n=project.PT(z,Rm[rmi[i]],n0[i],K[i],times)
    resid=(n[times]-y.obs)/n[times]
    residuals=c(residuals,resid)
  }
  if(debug)
  {
    cat("-lnl =",sum(residuals^2),"\n")
    cat("z =",z,"\n")
    cat("Rm =",Rm,"\n")
    cat("n0 =",n0,"\n")
    cat("K =",K,"\n")
  }
  return(residuals)
}

fit_gl=function(x,start=1975,z=c(1,20,1),Rm=0.10,Rm_byregion=FALSE,Rm_bystock=FALSE,MaxIter=20000,warnOnly=FALSE)
{
  #use first and last count as starting values for n0 and K
  n0=sapply(split(x$Count,x$Region),function(x) x[1])
  K=sapply(split(x$Count,x$Region),function(x) max(x))
  # for Rm set starting values based on RM_byregion
  if(Rm_bystock)Rm_byregion=FALSE
  if(Rm_byregion&length(Rm)==1)
    Rm=rep(Rm,length(levels(x$Region)))
  if(Rm_bystock&length(Rm)==1)
    Rm=rep(Rm,length(levels(x$Stock)))
  x$Rmindex=1
  if(Rm_byregion)x$Rmindex=as.numeric(x$Region)
  if(Rm_bystock)x$Rmindex=as.numeric(x$Stock)
  # parameter limits
  lower=c(z[1],rep(0,length(Rm)),n0/100,K/2)
  upper=c(z[2],rep(.3,length(Rm)),K,K*2)
  # z starting value
  z=z[3]
  #fit model
  dfm = data.frame(y.obs=x$Count,time.index=x$Year-start+1,region=x$Region,rmindex=x$Rmindex)
  glmod=nls(~nls.PT.mod.pe(z,Rm,n0,K,y.obs,time.index,region,rmindex), data=dfm,start=list(z=z,Rm=Rm,n0=n0,K=K),algorithm="port",
        upper=upper,lower=lower,  nls.control(maxit=MaxIter,warnOnly=warnOnly))

  return(glmod)
}


#read in results files for 7 regions and NI and C
SJFresults=read.csv("SJFResults.csv")
SJIresults=read.csv("SJIResults.csv")
EBresults=read.csv("EBResults.csv")
NIresults=read.csv("NIResults.csv")
HCresults=read.csv("HCResults.csv")
SPSresults=read.csv("SPSResults.csv")
OCresults=read.csv("OCResults.csv")
CEresults=read.csv("CEResults.csv")
Cresults=read.csv("CResults.csv")

debug=FALSE
# all counts with a constant Rm; if varies by stock or region - it will fail because of Hood Canal lack of signal
all_counts=rbind(cbind(SJFresults[,1:2],Region="Strait of Juan de Fuca",Stock="Northern Inland"),
                 cbind(SJIresults[,1:2],Region="San Juan Islands",Stock="Northern Inland"),
                 cbind(EBresults[,1:2],Region="Eastern Bays",Stock="Northern Inland"),
                 cbind(CEresults[,1:2],Region="Coastal Estuaries",Stock="Coastal"),
                 cbind(OCresults[,1:2],Region="Outer Coast",Stock="Coastal"),
                 cbind(HCresults[,1:2],Region="Hood Canal",Stock="Hood Canal"),
                 cbind(SPSresults[,1:2],Region="Southern Puget Sound",Stock="Southern Puget Sound"))
glmod=fit_gl(all_counts)


# exclude Hood Canal to explore region and stock differences
all_counts=rbind(cbind(SJFresults[,1:2],Region="Strait of Juan de Fuca",Stock="Northern Inland"),
                 cbind(SJIresults[,1:2],Region="San Juan Islands",Stock="Northern Inland"),
                 cbind(EBresults[,1:2],Region="Eastern Bays",Stock="Northern Inland"),
                 cbind(CEresults[,1:2],Region="Coastal Estuaries",Stock="Coastal"),
                 cbind(OCresults[,1:2],Region="Outer Coast",Stock="Coastal"),
                 cbind(SPSresults[,1:2],Region="Southern Puget Sound",Stock="Southern Puget Sound"))

glmod0=fit_gl(all_counts)
glmod1=fit_gl(all_counts,Rm_bystock=TRUE)
glmod2=fit_gl(all_counts,Rm_byregion=TRUE)

aic0=AIC(glmod0)
aic1=AIC(glmod1)
aic2=AIC(glmod2)

start=1975
end=max(all_counts$Year)
par_N=coef(glmod0)

#relative MNPL/K = (1+z)^-1(1/z) so MNPL=K*(1+z)^-1(1/z)
relMNPL=(par_N[1]+1)^(-1/par_N[1])
MNPL=c(NorthernInland=sum(par_N[9:11]),Coastal=sum(par_N[12:13]),SPugetSound=sum(par_N[14]))*relMNPL

predSJF=project.PT(par_N[1],par_N[2],par_N[3],par_N[9],1:(end-start+1))
predSJI=project.PT(par_N[1],par_N[2],par_N[4],par_N[10],1:(end-start+1))
predEB=project.PT(par_N[1],par_N[2],par_N[5],par_N[11],1:(end-start+1))
predCE=project.PT(par_N[1],par_N[2],par_N[6],par_N[12],1:(end-start+1))
predOC=project.PT(par_N[1],par_N[2],par_N[7],par_N[13],1:(end-start+1))
predSPS=project.PT(par_N[1],par_N[2],par_N[8],par_N[14],1:(end-start+1))

#regional coefficient of variation
cSJF=sqrt(mean(((predSJF[SJFresults[,1]-start+1]-SJFresults[,2])/predSJF[SJFresults[,1]-start+1])^2))
cSJI=sqrt(mean(((predSJI[SJIresults[,1]-start+1]-SJIresults[,2])/predSJI[SJIresults[,1]-start+1])^2))
cEB=sqrt(mean(((predEB[EBresults[,1]-start+1]-EBresults[,2])/predEB[EBresults[,1]-start+1])^2))
cCE=sqrt(mean(((predCE[CEresults[,1]-start+1]-CEresults[,2])/predCE[CEresults[,1]-start+1])^2))
cOC=sqrt(mean(((predOC[OCresults[,1]-start+1]-OCresults[,2])/predOC[OCresults[,1]-start+1])^2))
cSPS=sqrt(mean(((predSPS[SPSresults[,1]-start+1]-SPSresults[,2])/predSPS[SPSresults[,1]-start+1])^2))

parametric_bootstrap_model=function()
{
  # construct new dataframe by resampling residuals within region and 
  # SJF
  newresid=rnorm(nrow(SJFresults),0,cSJF)
  newcounts=(newresid+1)*predSJF[SJFresults[,1]-start+1]
  df=data.frame(Year=SJFresults[,1],Count=(newresid+1)*predSJF[SJFresults[,1]-start+1])
  index=nrow(df)
  #SJI
  newresid=rnorm(nrow(SJIresults),0,cSJI)
  newcounts=(newresid+1)*predSJI[SJIresults[,1]-start+1]
  df=rbind(df,data.frame(Year=SJIresults[,1],Count=(newresid+1)*predSJI[SJIresults[,1]-start+1]))
  index=nrow(df)
  #EB
  newresid=rnorm(nrow(EBresults),0,cEB)
  newcounts=(newresid+1)*predEB[EBresults[,1]-start+1]
  df=rbind(df,data.frame(Year=EBresults[,1],Count=(newresid+1)*predEB[EBresults[,1]-start+1]))
  index=nrow(df)
  #CE
  newresid=rnorm(nrow(CEresults),0,cCE)
  newcounts=(newresid+1)*predCE[CEresults[,1]-start+1]
  df=rbind(df,data.frame(Year=CEresults[,1],Count=(newresid+1)*predCE[CEresults[,1]-start+1]))
  index=nrow(df)
  #OC
  newresid=rnorm(nrow(OCresults),0,cOC)
  newcounts=(newresid+1)*predOC[OCresults[,1]-start+1]
  df=rbind(df,data.frame(Year=OCresults[,1],Count=(newresid+1)*predOC[OCresults[,1]-start+1]))
  index=nrow(df)
  #SPS
  newresid=rnorm(nrow(SPSresults),0,cSPS)
  newcounts=(newresid+1)*predSPS[SPSresults[,1]-start+1]
  df=rbind(df,data.frame(Year=SPSresults[,1],Count=(newresid+1)*predSPS[SPSresults[,1]-start+1]))
  df$Region=all_counts$Region
  df$Stock=all_counts$Stock
  # fit model to bootstrapped data
  glmod_bs=fit_gl(df,warnOnly=FALSE,MaxIter=100000)
  # get coefficients
  par_N=coef(glmod_bs)
  #relative MNPL/K = (1+z)^-1(1/z) so MNPL=K*(1+z)^-1(1/z)
  # get sample from normal distribution for cf
  rvcf=rnorm(1,cf,cf*cvcf)
  #relative MNPL/K = (1+z)^-1(1/z) so MNPL=K*(1+z)^-1(1/z)
  predNI=rvcf*(project.PT(par_N[1],par_N[2],par_N[3],par_N[9],1:(end-start+1)) +
                 project.PT(par_N[1],par_N[2],par_N[4],par_N[10],1:(end-start+1)) +
                 project.PT(par_N[1],par_N[2],par_N[5],par_N[11],1:(end-start+1)))
  predC=rvcf*(project.PT(par_N[1],par_N[2],par_N[6],par_N[12],1:(end-start+1)) +
                project.PT(par_N[1],par_N[2],par_N[7],par_N[13],1:(end-start+1)))
  predSPS=rvcf*project.PT(par_N[1],par_N[2],par_N[8],par_N[14],1:(end-start+1))
  relMNPL=(par_N[1]+1)^(-1/par_N[1])
  MNPL=c(NorthernInland=sum(par_N[9:11]),Coastal=sum(par_N[12:13]),SPugetSound=sum(par_N[14]))*relMNPL*rvcf
  currentN_MNPL=c(NIresults[nrow(NIresults),"Count"]*rvcf,Cresults[nrow(Cresults),"Count"]*rvcf,SPSresults[nrow(SPSresults),"Count"]*rvcf)/MNPL
  return(list(par_N=par_N,predNI=predNI,predC=predC,predSPS=predSPS,MNPL,currentN_MNPL))
}

debug=FALSE
#resid=residuals(glmod0)
nreps=100
bs_results=vector("list",length=nreps)
tryerror=0
i=0
while(i<nreps)
{
  cat("***** i = ",i,"\n")
  xx=try(parametric_bootstrap_model())
  if(class(xx)=="try-error")
    tryerror=tryerror+1
  else
  {
    i=i+1
    bs_results[[i]]=xx
  }
}

bspar_N=with(bs_results,sapply(bs_results,function(x)x[[1]]))
llpar_N=apply(bspar_N,1,function(x) sort(x)[nreps*0.025])
ulpar_N=apply(bspar_N,1,function(x) sort(x)[nreps*0.975])

bsNI=with(bs_results,sapply(bs_results,function(x)x[[2]]))
llNI=apply(bsNI,1,function(x) sort(x)[nreps*0.025])
ulNI=apply(bsNI,1,function(x) sort(x)[nreps*0.975])

bsC=with(bs_results,sapply(bs_results,function(x)x[[3]]))
llC=apply(bsC,1,function(x) sort(x)[nreps*0.025])
ulC=apply(bsC,1,function(x) sort(x)[nreps*0.975])

bsSPS=with(bs_results,sapply(bs_results,function(x)x[[4]]))
llSPS=apply(bsSPS,1,function(x) sort(x)[nreps*0.025])
ulSPS=apply(bsSPS,1,function(x) sort(x)[nreps*0.975])

bsMNPL=with(bs_results,sapply(bs_results,function(x)x[[5]]))
llMNPL=apply(bsMNPL,1,function(x) sort(x)[nreps*0.025])
ulMNPL=apply(bsMNPL,1,function(x) sort(x)[nreps*0.975])

bsMNPLratio=with(bs_results,sapply(bs_results,function(x)x[[6]]))
llMNPLratio=apply(bsMNPLratio,1,function(x) sort(x)[nreps*0.025])
ulMNPLratio=apply(bsMNPLratio,1,function(x) sort(x)[nreps*0.975])


pdf("Northern Inland GL.pdf")
layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))
with(all_counts[all_counts$Region=="Strait of Juan de Fuca",],
{
  plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predSJF)),max(c(Count,predSJF))),main="Strait of Juan de Fuca")
  lines(start:end,predSJF)
})
with(all_counts[all_counts$Region=="San Juan Islands",],
{
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predSJI)),max(c(Count,predSJI))),main="San Juan Islands")
   lines(start:end,predSJI)
})
with(all_counts[all_counts$Region=="Eastern Bays",],
{
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predEB)),max(c(Count,predEB))),main="Eastern Bays")
   lines(start:end,predEB)
})
with(NIresults,
{
   plot(Year,Abundance,xlim=c(start,end),ylim=c(min(c(Abundance,LCL,cf*(predSJF+predSJI+predEB))),max(c(Abundance,UCL,cf*(predSJF+predSJI+predEB)))),main="Northern Inland Stock")
   lines(start:end,cf*(predSJF+predSJI+predEB))
   lines(start:end,llNI,lty=2)
   lines(start:end,ulNI,lty=2)
   abline(cf*MNPL[1],0)
   abline(llMNPL[1],0,lty=3)
   abline(ulMNPL[1],0,lty=3)
})
for(i in 1:nrow(NIresults))
{
  x=c(NIresults$Year[i],NIresults$Year[i])
  y=c(NIresults$LCL[i],NIresults$UCL[i])
  lines(x,y)
}

dev.off()


pdf("Coastal Stock GL.pdf")
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
with(all_counts[all_counts$Region=="Coastal Estuaries",],
{
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predCE)),max(c(Count,predCE))),main="Coastal Estuaries")
   lines(start:end,predCE)
})
with(all_counts[all_counts$Region=="Outer Coast",],
{
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predOC)),max(c(Count,predOC))),main="Outer Coast")
   lines(start:end,predOC)
})
with(Cresults,
{
  plot(Year,Abundance,xlim=c(start,end),ylim=c(min(c(Abundance,cf*(predOC+predCE))),max(c(Abundance,UCL,cf*(predOC+predCE)))),main="Coastal Stock")
  lines(start:end,cf*(predOC+predCE))
  lines(start:end,llC,lty=2)
  lines(start:end,ulC,lty=2)
  abline(cf*MNPL[2],0)
  abline(llMNPL[2],0,lty=3)
  abline(ulMNPL[2],0,lty=3)
  
})
for(i in 1:nrow(Cresults))
{
  x=c(Cresults$Year[i],Cresults$Year[i])
  y=c(Cresults$LCL[i],Cresults$UCL[i])
  lines(x,y)
}
dev.off()

pdf("Southern Puget Sound Stock GL.pdf")
layout(1)
with(SPSresults,
{
   plot(Year,Abundance,xlim=c(start,end),ylim=c(min(c(Abundance,predSPS*cf,llSPS)),max(c(Abundance,predSPS*cf,ulSPS))),main="Southern Puget Sound")
   lines(start:end,predSPS*cf)
   lines(start:end,llSPS,lty=2)
   lines(start:end,ulSPS,lty=2)
   abline(cf*MNPL[3],0)
   abline(llMNPL[3],0,lty=3)
   abline(ulMNPL[3],0,lty=3)
})
for(i in 1:nrow(SPSresults))
{
  x=c(SPSresults$Year[i],SPSresults$Year[i])
  y=c(SPSresults$LCL[i],SPSresults$UCL[i])
  lines(x,y)
}
dev.off()

