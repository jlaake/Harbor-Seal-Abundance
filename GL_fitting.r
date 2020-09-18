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

fit_gl=function(x,start=1975,z=c(1,40,1),Rm=0.10,Rm_byregion=FALSE,Rm_bystock=FALSE,MaxIter=5000)
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
        upper=upper,lower=lower,  nls.control(maxit=MaxIter))

  return(glmod)
}


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

layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE))
with(all_counts[all_counts$Region=="Strait of Juan de Fuca",],
{
  predSJF<<-project.PT(par_N[1],par_N[2],par_N[3],par_N[9],1:(end-start+1))
  plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predSJF)),max(c(Count,predSJF))),main="Strait of Juan de Fuca")
  lines(start:end,predSJF)
})
with(all_counts[all_counts$Region=="San Juan Islands",],
{
   predSJI<<-project.PT(par_N[1],par_N[2],par_N[4],par_N[10],1:(end-start+1))
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predSJI)),max(c(Count,predSJI))),main="San Juan Islands")
   lines(start:end,predSJI)
})
with(all_counts[all_counts$Region=="Eastern Bays",],
{
   predEB<<-project.PT(par_N[1],par_N[2],par_N[5],par_N[11],1:(end-start+1))
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predEB)),max(c(Count,predEB))),main="Eastern Bays")
   lines(start:end,predEB)
})
with(NIresults,
{
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predSJF+predSJI+predEB)),max(c(Count,predSJF+predSJI+predEB))),main="Northern Inland Stock")
   lines(start:end,predSJF+predSJI+predEB)
})


layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
with(all_counts[all_counts$Region=="Coastal Estuaries",],
{
   predCE<<-project.PT(par_N[1],par_N[2],par_N[6],par_N[12],1:(end-start+1))
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predCE)),max(c(Count,predCE))),main="Coastal Estuaries")
   lines(start:end,predCE)
})
with(all_counts[all_counts$Region=="Outer Coast",],
{
   predOC<<-project.PT(par_N[1],par_N[2],par_N[7],par_N[13],1:(end-start+1))
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predOC)),max(c(Count,predOC))),main="Outer Coast")
   lines(start:end,predOC)
})
with(Cresults,
{
  plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predOC+predCE)),max(c(Count,predOC+predCE))),main="Coastal Stock")
  lines(start:end,predOC+predCE)
})

layout(1)
with(SPSresults,
{
   predSPS<<-project.PT(par_N[1],par_N[2],par_N[8],par_N[14],1:(end-start+1))
  
   plot(Year,Count,xlim=c(start,end),ylim=c(min(c(Count,predSPS)),max(c(Count,predSPS))),main="Southern Puget Sound")
   lines(start:end,predSPS)
})
