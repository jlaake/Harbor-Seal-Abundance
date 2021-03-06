rm(list=ls())
cf=1.53
cvcf=0.065

project.PT <-function (z,Rm,n0,K,times)
{
  n=vector(mode="numeric",length=max(times))
  n[1]  = n0
  for(i in 2:max(times))
    n[i] = n[i-1]*(1+Rm*(1-(n[i-1]/(K))^z))
  return(n)
}
# function to place estimated parameters in most general structure (regional model 2)
# to allow model weighting
select_par=function(modelnum,model_list)
{
  if(modelnum==0)
  {
    par_N=model_list$glmod0$par
    par_N=c(par_N[1],rep(par_N[2],6),par_N[3:length(par_N)])
  }   
  else
    if(modelnum==1)
    {
      par_N=model_list$glmod1$par
      par_N=c(par_N[1],rep(par_N[2],3),rep(par_N[3],2),par_N[4],par_N[5:length(par_N)])
    } 
  else
    par_N=model_list$glmod2$par
  return(par_N)
}

MNPLfct=function(model_list)
{
  par_N=select_par(0,model_list)
  #relative MNPL/K = (1+z)^-1(1/z) so MNPL=K*(1+z)^-1(1/z)
  relMNPL=(par_N[1]+1)^(-1/par_N[1])
  MNPL=c(NorthernInland=sum(par_N[14:16]),Coastal=sum(par_N[17:18]),SPugetSound=sum(par_N[19]))*relMNPL
  
  par_N=select_par(1,model_list)
  #relative MNPL/K = (1+z)^-1(1/z) so MNPL=K*(1+z)^-1(1/z)
  relMNPL=(par_N[1]+1)^(-1/par_N[1])
  MNPL=cbind(MNPL,c(NorthernInland=sum(par_N[14:16]),Coastal=sum(par_N[17:18]),SPugetSound=sum(par_N[19]))*relMNPL)
  
  par_N=select_par(2,model_list)
  #relative MNPL/K = (1+z)^-1(1/z) so MNPL=K*(1+z)^-1(1/z)
  relMNPL=(par_N[1]+1)^(-1/par_N[1])
  MNPL=cbind(MNPL,c(NorthernInland=sum(par_N[14:16]),Coastal=sum(par_N[17:18]),SPugetSound=sum(par_N[19]))*relMNPL)
  return(as.vector(MNPL%*%model_list$model_weights)) 
}

predictions=function(region,model_list)
{
  par_N=select_par(0,model_list)
  pred=project.PT(par_N[1],par_N[region+1],par_N[region+7],par_N[region+13],1:(end-start+1))
  par_N=select_par(1,model_list)
  pred=cbind(pred,project.PT(par_N[1],par_N[region+1],par_N[region+7],par_N[region+13],1:(end-start+1)))
  par_N=select_par(2,model_list)
  pred=cbind(pred,project.PT(par_N[1],par_N[region+1],par_N[region+7],par_N[region+13],1:(end-start+1)))
  return(as.vector(pred%*%model_list$model_weights))
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

# exclude Hood Canal to explore region and stock differences
all_counts=rbind(cbind(SJFresults[,1:2],Region="Strait of Juan de Fuca",Stock="Northern Inland"),
                 cbind(SJIresults[,1:2],Region="San Juan Islands",Stock="Northern Inland"),
                 cbind(EBresults[,1:2],Region="Eastern Bays",Stock="Northern Inland"),
                 cbind(CEresults[,1:2],Region="Coastal Estuaries",Stock="Coastal"),
                 cbind(OCresults[,1:2],Region="Outer Coast",Stock="Coastal"),
                 cbind(SPSresults[,1:2],Region="Southern Puget Sound",Stock="Southern Puget Sound"))



load(file="gl_models.rda")

start=1975
end=max(all_counts$Year)

model_list=list(glmod0=glmod0,glmod1=glmod1,glmod2=glmod2,model_weights=model_weights)


predSJF=predictions(1,model_list)
predSJI=predictions(2,model_list)
predEB=predictions(3,model_list)
predCE=predictions(4,model_list)
predOC=predictions(5,model_list)
predSPS=predictions(6,model_list)

load(file="gl_bootstraps.rda")
nreps=length(bs_results)

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


pdf("Northern_Inland_GL.pdf")
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


pdf("Coastal_Stock_GL.pdf")
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

pdf("Southern_Puget_Sound_Stock_GL.pdf")
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

