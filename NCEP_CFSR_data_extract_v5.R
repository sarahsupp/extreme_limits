# code and functions for Extreme Limits manuscript (PAB, TAC, SRS)
# Starts with Mexico_SOT_level3.rdata 
# Makes Mexico_CFSR_level2.rdata (?)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a function to summarize extracted data per day, sum/mean/max/min ++++++++++++++++####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  dailyCFSRvals <- function(CFSR.dat,FUN="mean",varname,selectEpoch=NULL){
  
  if (!is.null(selectEpoch)){if (any(CFSR.dat$imEpoch > 4)){cat('imEpoch exceeds 4 \n');browser()}
  CFSR.dat <- CFSR.dat[is.element(CFSR.dat$imEpoch,selectEpoch),]
  cat('CFSR data was subsetted and daily statistics will based on epochs: ',selectEpoch,'\n')
  }
    #preserve the cells that are represented/sampled multiple times
    if(!is.element(varname,colnames(CFSR.dat))){cat(paste('no column named ',varname,' in CFSR.dat'));browser()}
    CFSR.dat <- CFSR.dat[,c('imdate','cellnrs',varname)]
    
 CFSRdat.dat.daily.1<-aggregate(CFSR.dat[varname],by=list(CFSR.dat$imdate,CFSR.dat$cellnrs),FUN=get(FUN[1]))
 if (length(FUN)==2){
   CFSRdat.dat.daily.2<-aggregate(CFSR.dat[varname],by=list(CFSR.dat$imdate,CFSR.dat$cellnrs),FUN=get(FUN[2]))
 }
 CFSRdat.dat.daily<-aggregate(CFSR.dat[,-1],by=list(CFSR.dat$imdate,CFSR.dat$cellnrs),FUN=function(x){x[1]}) 
 CFSRdat.dat.daily[,4]<-CFSRdat.dat.daily.1[varname]
 if (length(FUN)==2){
    #CFSRdat.dat.daily.2<-aggregate(CFSR.dat[,4],by=list(CFSR.dat$imdate,CFSR.dat$cellnrs),FUN=get(FUN[2]))
    CFSRdat.dat.daily.2 <- CFSRdat.dat.daily.2[varname]
    CFSRdat.dat.daily <- cbind.data.frame(CFSRdat.dat.daily,CFSRdat.dat.daily.2)}  
 #CFSRdat.dat.daily <- CFSRdat.dat.daily[,-c(1,4)]
    CFSRdat.dat.daily <- CFSRdat.dat.daily[,-2]
 colnames(CFSRdat.dat.daily)[1]<-"imdate"
 return(CFSRdat.dat.daily)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a function to ammend a d.f. with years ndays since start day of choice +++++++++++####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  days.since.base<-function(mydates,baseline="-01-01") {
    outdat<-matrix(NA,nrow=length(mydates),ncol=2)
  goods <- which(is.na(mydates)==F)
  mydates <- mydates[goods]
  my.years<-as.numeric(substr(mydates,1,4))
  prev.baseline <- as.Date(paste(my.years-1,baseline,sep=""))
  nr.of.days.in.year <- as.numeric(as.Date(paste(my.years-1,"-12-31",sep=""))-as.Date(paste(my.years-1,"-01-01",sep="")))+1
  days.since <- as.numeric(mydates-prev.baseline)
  my.years[days.since>=nr.of.days.in.year]<-my.years[days.since>=nr.of.days.in.year]+1
  my.years<-my.years-1
  days.since <- days.since%%nr.of.days.in.year
  outdat[goods,]<-cbind(days.since,my.years)
  colnames(outdat)<-c('DOYsinceBase','BaseYear')
  return(outdat)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a function to calculate cumulative CFSR values since a chosen date
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Cumvals.dailyCFSR <- function(CFSR.dat,Varname){
  invar <- CFSR.dat[[Varname]]
 outcum <- invar*NA
  cellfac <- levels(as.factor(CFSR.dat$cellnr))
 for (i. in 1:length(cellfac)){
   i<-cellfac[i.]
   for (j in levels(as.factor(CFSR.dat$BaseYear))){
     ss <- which((CFSR.dat$cellnr==i)&(CFSR.dat$BaseYear==j))
     outvals <- cumsum(invar[ss])
     outcum[ss] <- outvals
   }
 cat(i.,"cells processed,",length(cellfac)-i.," to go\n")
 }
  
  outcum <- as.data.frame(outcum);colnames(outcum)<-paste('cum',Varname,sep="")
  return(outcum)}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# a function to calculate daily statistics of CFRS variables
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Daily.stat.CFSR <- function(CFSR.dat.d,Varnames=NULL){
  if(is.null(Varnames)){cat("Daily.stat.CFSR needs Varnames to be specified\n");browser()}
  datcols <- as.data.frame(CFSR.dat.d[,is.element(colnames(CFSR.dat.d),Varnames)])
  colnames(datcols) <- colnames(CFSR.dat.d[is.element(colnames(CFSR.dat.d),Varnames)])
  CFSRdat.mn<-aggregate(datcols,by=list(CFSR.dat.d$imdate),FUN=mean)
  #CFSRdat.mn.days <- CFSRdat.mn[,1]
  CFSRdat.mn<-as.data.frame(CFSRdat.mn[,-1])
  colnames(CFSRdat.mn)<-paste(colnames(datcols),"_mn",sep="")
  
  #CFSRdat.sd.days <- CFSRdat.mn[,1]
  CFSRdat.sd<-aggregate(datcols,by=list(CFSR.dat.d$imdate),FUN=sd)
  CFSRdat.sd <- as.data.frame(CFSRdat.sd[,-1])
  colnames(CFSRdat.sd)<-paste(colnames(datcols),"_sd",sep="")
  
  
  CFSRdat.dat.daily<-aggregate(CFSR.dat.d[,-1],by=list(CFSR.dat.d$imdate),FUN=function(x){x[1]}) 
  CFSRdat.dat.daily <- CFSRdat.dat.daily[,!is.element(colnames(CFSR.dat.d),Varnames)]
  CFSRdat.dat.daily <- cbind.data.frame(CFSRdat.dat.daily,CFSRdat.mn,CFSRdat.sd)
  colnames(CFSRdat.dat.daily)[1] <- 'imdate'
  
  return(CFSRdat.dat.daily)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#read in the Mexico site data
#source('C:\\Users\\pbeck.WHRC\\Dropbox\\green_wave_surfing\\stack_RunMrt_produced_tifs.R')
#source("C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\read_Mex_obs.r")
#site.vec<-site.vec[site.vec$wint.high.elev==T,]



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Load the met data extracted during the calculation of Stand. Op. Temp
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#load("A:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\Mexico_SOT_level3.rdata")
load("C:/Users/sarah/Documents/GitHub/extreme_limits/data/Mexico_SOT_level3.rdata")
cn <- colnames(allmet)
cn[cn=="cam_pre"] <- "pre"
colnames(allmet) <- cn;rm(cn)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Process the rain data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#CFSR.Pre_6hr.dir <- 'A:\\data\\cfsr\\cam_Pre_6hr'
#Prec.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Pre_6hr.dir,site.vec,start.date="2000-01-01",end.date="2012-01-03",Varname="pre",xtrachar=4)
Prec.dat <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("sum"),varname='pre')
#Prec.dat <- dailyCFSRvals(CFSR.dat=Prec.dat,FUN="sum")
Prec.dat.d <- cbind.data.frame(Prec.dat,days.since.base(Prec.dat$imdate,baseline="-11-01"))
Prec.dat.d <- cbind.data.frame(Prec.dat.d,Cumvals.dailyCFSR(Prec.dat.d,"pre"))

plot(Prec.dat.d$imdate,Prec.dat.d$pre,col=Prec.dat.d$BaseYear)
#at this stage, using ...dat.d you can do interannual stats WRITE FUNCTION FOR IT
Prec.dat.d.stats <- Daily.stat.CFSR(Prec.dat.d,Varnames=c('pre','cumpre'))
plot(Prec.dat.d.stats$imdate,Prec.dat.d.stats$pre_mn,col=Prec.dat.d.stats$BaseYear,cex=.1,pch=16)
plot(Prec.dat.d.stats$imdate,Prec.dat.d.stats$cumpre_mn,col=Prec.dat.d.stats$BaseYear,cex=.1,pch=16)

#dat.d.stats can be used for good levelplots WRITE FUNCTION FOR IT!

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Process the tmin data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#CFSR.Tmin_6hr.dir <- 'A:\\data\\cfsr\\cam_Tmin_6hr'
#Tmin.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Tmin_6hr.dir,site.vec,start.date="2000-01-01",end.date="2012-01-03",Varname="tmin",xtrachar=4)
#Tmin.dat <- dailyCFSRvals(CFSR.dat=Tmin.dat,FUN="mean")
Tmin.dat.d <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("mean"),varname="tmin")
#colnames(Tmin.dat.d)[ncol(Tes.dat.d)] <- "mean"
Tmin.dat.d <- cbind.data.frame(Tmin.dat.d,days.since.base(Tmin.dat.d$imdate,baseline="-11-01"))
#Tmin.dat.d <- cbind.data.frame(Tmin.dat.d,Cumvals.dailyCFSR(Tes.dat.d,"epochSub122"))
plot(Tmin.dat.d$imdate,Tmin.dat.d$tmin,col=Tmin.dat.d$BaseYear)
#at this stage using.dat.d you can do interannual stats WRITE FUNCTION FOR IT
Tmin.dat.d.stats <- Daily.stat.CFSR(Tmin.dat.d,Varnames=c('tmin'))
#dat.d.stats can be used for good levelplots WRITE FUNCTION FOR IT!
#plot(Tes.dat.d.stats$imdate,Tes.dat.d.stats$cumepochSub122_mn,col=Tes.dat.d.stats$BaseYear,pch=16,cex=.1)
plot(Tmin.dat.d.stats$imdate,Tmin.dat.d.stats$tmin_mn,col=Tmin.dat.d.stats$BaseYear,pch=16,cex=.1)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Process the tmax data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Tmax.dat.d <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("mean"),varname="tmax")
#colnames(Tmin.dat.d)[ncol(Tes.dat.d)] <- "mean"
Tmax.dat.d <- cbind.data.frame(Tmax.dat.d,days.since.base(Tmax.dat.d$imdate,baseline="-11-01"))
#Tmin.dat.d <- cbind.data.frame(Tmin.dat.d,Cumvals.dailyCFSR(Tes.dat.d,"epochSub122"))
plot(Tmax.dat.d$imdate,Tmax.dat.d$tmin,col=Tmax.dat.d$BaseYear)
#at this stage using.dat.d you can do interannual stats WRITE FUNCTION FOR IT
Tmax.dat.d.stats <- Daily.stat.CFSR(Tmax.dat.d,Varnames=c('tmax'))
#dat.d.stats can be used for good levelplots WRITE FUNCTION FOR IT!
#plot(Tes.dat.d.stats$imdate,Tes.dat.d.stats$cumepochSub122_mn,col=Tes.dat.d.stats$BaseYear,pch=16,cex=.1)
plot(Tmax.dat.d.stats$imdate,Tmax.dat.d.stats$tmax_mn,col=Tmax.dat.d.stats$BaseYear,pch=16,cex=.1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Process the Te data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Te.dat.d <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("min"),varname="Te")
#colnames(Tmin.dat.d)[ncol(Tes.dat.d)] <- "mean"
Te.dat.d <- cbind.data.frame(Te.dat.d,days.since.base(Te.dat.d$imdate,baseline="-11-01"))
#Tmin.dat.d <- cbind.data.frame(Tmin.dat.d,Cumvals.dailyCFSR(Tes.dat.d,"epochSub122"))
#plot(Te.dat.d$imdate,Te.dat.d$tmin,col=Te.dat.d$BaseYear)
#at this stage using.dat.d you can do interannual stats WRITE FUNCTION FOR IT
Te.dat.d.stats <- Daily.stat.CFSR(Te.dat.d,Varnames=c('Te'))
win.graph()
plot(Te.dat.d.stats$DOYsinceBase,Te.dat.d.stats$Te_mn,col=Te.dat.d.stats$BaseYear,pch=16,cex=.1)
plot(Te.dat.d.stats$imdate,Te.dat.d.stats$Te_mn,col=Te.dat.d.stats$BaseYear,pch=16,cex=.1)

# 
# #CFSR.Tmin_6hr.dir <- 'A:\\data\\cfsr\\cam_Tmin_6hr'
# #Tmin.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Tmin_6hr.dir,site.vec,start.date="2000-01-01",end.date="2012-01-03",Varname="tmin",xtrachar=4)
# below.12.2 <- function(x){length(which(x <= 12.2))}
# allbelow.10 <- function(x){ifelse(max(x)<= 10,1,0)}
# 
# #Tmin.dat <- dailyCFSRvals(CFSR.dat=Tmin.dat,FUN="mean")
# Te.dat.d <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("allbelow.10"),varname="Te",selectEpoch=c(1,2))
# Te.dat.d <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("allbelow.10"),varname="Te")
# 
# cn <- colnames(Te.dat.d)
# cn[cn=="Te"] <- "allbelow10"
# colnames(Te.dat.d) <- cn;rm(cn)
# Te.dat.d <- cbind.data.frame(Te.dat.d,days.since.base(Te.dat.d$imdate,baseline="-11-01"))
# Te.dat.d <- cbind.data.frame(Te.dat.d,Cumvals.dailyCFSR(Te.dat.d,"allbelow10"))

#plot(Te.dat.d$imdate,Te.dat.d$cumallbelow10,col=Te.dat.d$BaseYear)
#at this stage using.dat.d you can do interannual stats WRITE FUNCTION FOR IT
#Te.dat.d.stats <- Daily.stat.CFSR(Te.dat.d,Varnames='cumallbelow10')

Te.dat.d.stats <- Daily.stat.CFSR(Te.dat.d,Varnames=c('Te','cumallbelow10'))
#dat.d.stats can be used for good levelplots WRITE FUNCTION FOR IT!
#plot(Tes.dat.d.stats$imdate,Tes.dat.d.stats$cumepochSub122_mn,col=Tes.dat.d.stats$BaseYear,pch=16,cex=.1)
plot(Te.dat.d.stats$imdate,Te.dat.d.stats$cumallbelow10_mn,col=Te.dat.d.stats$BaseYear,pch=16,cex=.1)
plot(Te.dat.d.stats$DOYsinceBase,Te.dat.d.stats$allbelow10_mn,col=Te.dat.d.stats$BaseYear,pch=16,cex=.1)
#save(Te.dat.d.stats,Te.dat.d,file="A:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\tempfile2.rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Process the Tes data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#CFSR.Tmin_6hr.dir <- 'A:\\data\\cfsr\\cam_Tmin_6hr'
#Tmin.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Tmin_6hr.dir,site.vec,start.date="2000-01-01",end.date="2012-01-03",Varname="tmin",xtrachar=4)
#below.12.2 <- function(x){length(which(x <= 12.2))}

#Tmin.dat <- dailyCFSRvals(CFSR.dat=Tmin.dat,FUN="mean")
Tes.dat.d <- dailyCFSRvals(CFSR.dat=allmet,FUN=c("mean"),varname="Tes")
#colnames(Tes.dat.d)[ncol(Tes.dat.d)] <- "epochSub122"
Tes.dat.d <- cbind.data.frame(Tes.dat.d,days.since.base(Tes.dat.d$imdate,baseline="-11-01"))
#Tes.dat.d <- cbind.data.frame(Tes.dat.d,Cumvals.dailyCFSR(Tes.dat.d,"epochSub122"))
#plot(Tes.dat.d$imdate,Tes.dat.d$cumepochSub122,col=Tes.dat.d$BaseYear)
#at this stage using.dat.d you can do interannual stats WRITE FUNCTION FOR IT
Tes.dat.d.stats <- Daily.stat.CFSR(Tes.dat.d,Varnames='Tes')
#dat.d.stats can be used for good levelplots WRITE FUNCTION FOR IT!
#plot(Tes.dat.d.stats$imdate,Tes.dat.d.stats$cumepochSub122_mn,col=Tes.dat.d.stats$BaseYear,pch=16,cex=.1)
plot(Tes.dat.d.stats$imdate,Tes.dat.d.stats$Tes_mn,col=Tes.dat.d.stats$BaseYear,pch=16,cex=.1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
rm(Prec.dat,Cld.dat,Tmin.dat,Wnd.dat)
#save.image("C:\\Data\\WHRC\\Hummers\\R_data\\Mexico_CFSR_level2.rdata")
#save a copy on Arctic for heavier analysis there
save.image("A:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\Mexico_CFSR_level2.rdata")
