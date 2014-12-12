
#code to run on summarized winter data
#+++++++++++ functions to perform and analyze Tukey test  +++++++++++####
plot.CFSR.Annual.var <- function(df,maxday=150,varname,maxbaseyr=2011){
  ss<-which((df$BaseYear >1999)&(df$DOYsinceBase < maxday)&(df$BaseYear <= maxbaseyr))
  win.graph(width=9,h=5)
  plot(as.factor(df$BaseYear[ss]),df[[varname]][ss],ylab=varname,col="grey")
  Tukeytab <- TukeyTable(classes=as.factor(df$BaseYear[ss]),dependent=df[[varname]][ss],compnames=
                           c(paste('higher ',varname,sep=''),'no diff.',paste('lower ',varname,sep='')))
  return(Tukeytab)
}

comp <- function(year,ttt){
  list(which(substr(rownames(ttt),1,4)==as.character(year)),
       which (substr(rownames(ttt),6,9)==as.character(year)))}

TukeyTable <- function(classes,dependent,compnames,alpha=0.05){
  classes<-droplevels(classes)
  #tt<-TukeyHSD(aov(allNDVI[ss]~years.from.baseline.fac[ss]))
  tt<-TukeyHSD(aov(dependent~classes))
  ttt<-tt$classes
  # a function to summarize the output from TukeyHSD
  
  allcomps<-NULL
  for(i in levels(classes)){
    signif <- c(ttt[comp(i,ttt=ttt)[[1]],4]<alpha,ttt[comp(i,ttt=ttt)[[2]],4]<alpha)
    diffs <- c(ttt[comp(i,ttt=ttt)[[1]],1]>0,ttt[comp(i,ttt=ttt)[[2]],1]<0)
    newcol <- c(length(which(diffs==T & signif==T)),
                length(which(signif==F)),
                length(which(diffs==F & signif ==T)))
    allcomps<-cbind(allcomps,newcol)}
  colnames(allcomps) <- levels(classes)
  rownames(allcomps) <- compnames
  return(allcomps)}

#+++++++++++ a simple plot for yearly time series +++++++++++++++++####

Simple.plot <- function(CFSR.dat.d.stats,Varname=NULL,minbaseyr=2000,maxbaseyr=2011,maxday=500){
  df<-CFSR.dat.d.stats
  df<-df[(is.element(df$BaseYear,c(minbaseyr:maxbaseyr))&(df$DOYsinceBase < maxday)),]
  require(lattice)
  win.graph()
  xyplot(get(Varname) ~ DOYsinceBase, data = df,ylab=Varname,lwd=2,
         groups = as.factor(BaseYear),type='smooth',span=.3,auto.key=T)
  #groups = as.factor(BaseYear),span=.3,auto.key=T)
}

#+++++++++++ calculate daily anomalies per DOY and cellnr for CFSR data +++++++++++++++++####

Daily.anom.stats <- function(CFSR.dat.d,Varname=NULL,minbaseyr=2000,maxbaseyr=2011,maxday=500){
  #restrict the data set as requested
  df<-CFSR.dat.d
  df<-df[(is.element(df$BaseYear,c(minbaseyr:maxbaseyr))&(df$DOYsinceBase < maxday)),]
  #calculate the long-term daily means per cell
  long.term.daily.mn <- aggregate(df,by=list(substr(df$imdate,6,10),df$cellnrs),FUN=mean)
  #convert the observations to anomalies from the long-term cell&day-level means
  long.term.daily.mn$DOYsinceBase <- ceiling(long.term.daily.mn$DOYsinceBase)
  require(plyr)
  df3 <- join(df,long.term.daily.mn,by=c("DOYsinceBase","cellnrs"),type="left",match="first")
  #df3 <- df3[,!duplicated(colnames(df3))]
  vardf <- df3[,colnames(df3)==Varname]
  anomname <- paste(Varname,"_anom",sep="")
  anom <- vardf[,1]-vardf[,2]
  df[[anomname]]<-anom
  
  return(df)  
}

#++++++++###+++++++++++ load output of NCEP_CFSR_data_extract.R+++++++++++++++++++####

#load("C:\\Data\\WHRC\\Hummers\\R_data\\Mexico_CFSR_level2.rdata")
#load("C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\Mexico_CFSR_level2.rdata")
#load("A:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\Mexico_CFSR_level2.rdata")
load("C:/Users/sarah/Documents/GitHub/extreme_limits/data/Mexico_CFSR_level2.rdata")

# Cld.dat.d;Cld.data.d.stats
# Prec.dat.d;Prec.data.d.stats
# Tmin.dat.d;Tmin.dat.d.stats
# Wnd.dat.d;Wnd.data.d.stats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#lines of yearly NDVI and cumulative precip
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
    precip.fac <- 6*60*60
#    maxDOY<-max(annual.anom.df$DOYsinceBase.y)   
#    rainbowcols<-rainbow(maxDOY*5/4)[1:(maxDOY+1)]

#Simple smoothed plots of annual NCEP CFSR ts +++++++++++++++++++####

Simple.plot(Tes.dat.d.stats,"Tes_mn",minbaseyr=2004,maxday=250)
Simple.plot(Prec.dat.d.stats,"cumpre_mn",minbaseyr=2004,maxday=250)
#Simple.plot(Wnd.dat.d.stats,"wnd_mn",minbaseyr=2004,maxday=250)
#Simple.plot(Cld.dat.d.stats,"cld_mn",minbaseyr=2004,maxday=250)
Simple.plot(Tmin.dat.d.stats,"tmin_mn",minbaseyr=2004,maxday=250)
#Simple.plot(Te.dat.d.stats,"cumallbelow10_mn",minbaseyr=2004,maxbaseyr=2010,maxday=133) #this doesn't get createdin NCEP_CFSR... version that I have - do we need it?

#Tukey tests of yearly means - Can you do this on ts in non-paired fashion? +++++++++++++++++++####
Tukey.cumpre<-plot.CFSR.Annual.var(Prec.dat.d,maxday=150,varname="pre")
Tukey.tmin<-plot.CFSR.Annual.var(Tmin.dat.d,maxday=150,varname="tmin")
#Tukey.wnd<-plot.CFSR.Annual.var(Wnd.dat.d,maxday=150,varname="wnd")
#Tukey.cld<-plot.CFSR.Annual.var(Cld.dat.d,maxday=150,varname="cld")

#anomalies: plot and summary statistics of daily NCEP CFSR  +++++++++++++++++++####
Tmin.dat.d <- Daily.anom.stats(Tmin.dat.d,Varname="tmin",minbaseyr=2000,maxbaseyr=2012,maxday=134)
Tukey.tesanom<-plot.CFSR.Annual.var(Tes.dat.d,maxday=134,varname="Tes_anom");abline(h=0)

Tes.dat.d <- Daily.anom.stats(Tes.dat.d,Varname="Tes",minbaseyr=2000,maxbaseyr=2012,maxday=134)
Tukey.tesanom<-plot.CFSR.Annual.var(Tes.dat.d,maxday=134,varname="Tes_anom");abline(h=0)

Prec.dat.d <- Daily.anom.stats(Prec.dat.d,Varname="cumpre",minbaseyr=2000,maxbaseyr=2012,maxday=134)
Tukey.cumpreanom <- plot.CFSR.Annual.var(Prec.dat.d,maxday=150,varname="cumpre_anom");abline(h=0)

#Te.dat.d <- Te.dat.d[,colnames(Te.dat.d)!="allbelow10"]
Te.dat.d <- Daily.anom.stats(Te.dat.d,Varname="Te",minbaseyr=2000,maxbaseyr=2012,maxday=134)
Tukey.cumpreanom <- plot.CFSR.Annual.var(Te.dat.d,maxday=150,varname="Te_anom");abline(h=0)

#Very 0-inflated distribution  - do cumulative?
#Wnd.dat.d <-  Daily.anom.stats(Wnd.dat.d,Varname="wnd",minbaseyr=2000,maxbaseyr=2012,maxday=250)
#  Tukey.wndanom<-plot.CFSR.Annual.var(Wnd.dat.d,maxday=150,varname="wnd_anom");abline(h=0)
#Cld.dat.d <- Daily.anom.stats(Cld.dat.d,Varname="cld",minbaseyr=2000,maxbaseyr=2012,maxday=250)
#  Tukey.cldanom <- plot.CFSR.Annual.var(Cld.dat.d,maxday=150,varname="cld_anom");abline(h=0)

#this df will hold yearly anomalies for meteo and NDVI over the focal period
annual.anom.df <- join(Tes.dat.d,Prec.dat.d)
annual.anom.df <- join(annual.anom.df,Tmin.dat.d)
annual.anom.df <- join(annual.anom.df,Te.dat.d)

#annual.anom.df <- join(annual.anom.df,Cld.dat.d)
#annual.anom.df <- join(annual.anom.df,Wnd.dat.d)
#make them daily (ie take averages across points on the same day)
annual.anom.df <- aggregate(annual.anom.df, by=list(annual.anom.df$imdate),FUN=mean)


#+++++++++++++++++++ load the NDVI d.f.+++++++++++++++++++####

#load(file="A:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\NDVI_df.rdata")
load("C:/Users/sarah/Documents/GitHub/extreme_limits/data/NDVI_df.rdata")

#NDVI.dat
NDVI.dat<-NDVI.dat[!duplicated(NDVI.dat),]
colnames(NDVI.dat)
#+++++++++++ make 1 smoothed NDVI ts  ++++++++++####
NDVI.dat.daily<-aggregate(NDVI.dat,by=list(NDVI.dat$allDate),FUN=mean)
NDVI.dat.daily$years.from.baseline.fac <- as.factor(NDVI.dat.daily$years.from.baseline)
NDVI.dat.daily$allYEARfac <- as.factor(NDVI.dat.daily$allYEAR)

aqf <- function(x,splinemod=spl) {
  zz <- predict(splinemod, x)$y;zz}
win.graph()
plot(NDVI.dat$allDate,NDVI.dat$allNDVI,pch=16,col=rgb(.5,.5,.5,.2),cex=.2,ylab="MODIS (Terra & Aqua) NDVI",
     xlab="",main="Vegetation productivity on Broad-tailed Hummingbird wintering grounds",
     ylim=c(.4,.9))#,xlim=as.Date(c())
spl <- smooth.spline(NDVI.dat$allDate,NDVI.dat$allNDVI,df=80)
prednrs <- c(min(spl$x):max(spl$x))
lines(as.Date(prednrs,origin="1970-01-01"),aqf(x=prednrs,splinemod=spl),col="dark green",lwd=2.5)
abline(v=as.Date(paste(c(2001:2013),"-01-01",sep="")),lty=2)

spl2 <- smooth.spline(NDVI.dat.daily$allDate,NDVI.dat.daily$allNDVI,df=80)
#plot(NDVI.dat.daily$allDate,NDVI.dat.daily$allNDVI,pch=16,col="grey",cex=.5)
lines(as.Date(prednrs,origin="1970-01-01"),aqf(x=prednrs,splinemod=spl2),col=4)

smoothNDVIdf <- cbind.data.frame(as.Date(prednrs,origin="1970-01-01"),aqf(x=prednrs,splinemod=spl))
colnames(smoothNDVIdf) <- c("imdate","smoothNDVI")
smoothNDVIdf <- cbind.data.frame(smoothNDVIdf,days.since.base(smoothNDVIdf$imdate,baseline="-11-01"))
smoothNDVIdf[["cellnrs"]]<-rep(-1,nrow(smoothNDVIdf))
smoothNDVIdf <- Daily.anom.stats(smoothNDVIdf,Varname="smoothNDVI",minbaseyr=2000,maxbaseyr=2012,maxday=250)
Tukey.NDVIanom <- plot.CFSR.Annual.var(smoothNDVIdf,maxday=150,varname="smoothNDVI_anom");abline(h=0)


#++++++++++++++++++++ A DF TO HOLD ANNUAL SERIES FOR MULTIPLE VARS ++++####smoothNDVIdf <- cbind.data.frame(as.Date(prednrs,origin="1970-01-01"),aqf(x=prednrs,splinemod=spl2))
annual.anom.df.backup <- annual.anom.df
#annual.anom.df<-annual.anom.df.backup

annual.anom.df<- merge(x=annual.anom.df,y=smoothNDVIdf,FUN=mean,by.x="Group.1",by.y="imdate")

#keep only the points in the focal years
annual.anom.df <- annual.anom.df[(annual.anom.df$BaseYear.x <= 2011)&(annual.anom.df$BaseYear.x >= 2000),]  #CHECK THE ONE-FIFTY NR

#save(annual.anom.df,file="A:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\annual.anom.df.rdata")
save(annual.anom.df, file="C:/Users/sarah/Documents/GitHub/extreme_limits/data/annual.anom.df.rdata")



# Simple smoothed plots of annual NDVI ts+++++++++++++++++++####
Simple.plot(smoothNDVIdf,"smoothNDVI",minbaseyr=2002,maxday=250)
Tukey.NDVI<-plot.CFSR.Annual.var(smoothNDVIdf,maxday=150,varname="smoothNDVI")
smoothNDVIdf <- Daily.anom.stats(smoothNDVIdf,Varname="smoothNDVI",minbaseyr=2000,maxbaseyr=2012,maxday=250)
Tukey.NDVI <- plot.CFSR.Annual.var(smoothNDVIdf,maxday=150,varname="smoothNDVI");abline(h=0)

#++++++++++++++++++++ A FUNCTION TO COMPARE 2 VARS BETWEEN ONE YEAR AND THE OTHERS ++++####
BivariateWinterComp<- function(df=annual.anom.df,xvar,yvar,maxDOY=250,focalBaseYr=2010,
                               Focalpoints=T,PointsLegend=T,focalBag=F,
                               xlabb=NULL,
                               ylabb=NULL,
                               xarrow=c(NA,NA),yarrow=c(NA,NA),legpos="topleft")
{
  require(aplpack)
  
  df<-df[df$DOYsinceBase.y<=maxDOY,]
  xx<-df[[xvar]];yy<-df[[yvar]]
  ss<-df$BaseYear.x==focalBaseYr
  bagplot(x=xx[!ss],y=yy[!ss],na.rm=T, xlab=xlabb,ylab=ylabb,
          show.outlier=T,show.bagpoints=F,show.looppoints=F,show.whiskers=F,
          transparency=T,xlim=range(xx,na.rm=T),ylim=range(yy,na.rm=T),cex=0.4
          ,col.loophull=grey(.75)
          ,col.baghull=grey(.5),col.bagpoints=1
  )
  cat(length(which(ss))," observations in focal year\n")
  cat(length(which(!ss))," observations in all other years focal year\n")
  if(focalBag){bagplot(x=xx[ss],y=yy[ss],na.rm=T, 
                       show.outlier=F,show.bagpoints=F,show.looppoints=F,show.whiskers=F,
                       transparency=T,xlim=range(xx,na.rm=T),ylim=range(yy,na.rm=T),cex=0,add=T)}
  rainbowcols<-rainbow(maxDOY*5/4)[1:(maxDOY+1)]
  
  if(Focalpoints){
    #points(xx[ss],yy[ss],pch=16,col=rainbow(df$DOYsinceBase.y[ss]/maxDOY))
    points(xx[ss],yy[ss],pch=16,col=rainbowcols[df$DOYsinceBase.y[ss]+1])
    #points(xx[ss],yy[ss])
  }  
  
  if(!is.na(yarrow[1])){
    arrows(x0=min(xx,na.rm=T),y0=min(yy,na.rm=T)/3*1,y1=min(yy,na.rm=T)/3*2,length=.1)
    mtext(side=2,line=-1,at=max(yy,na.rm=T)/2,text=yarrow[2])
    arrows(x0=min(xx,na.rm=T),y0=max(yy,na.rm=T)/3*1,y1=max(yy,na.rm=T)/3*2,length=.1)
    mtext(side=2,line=-1,at=min(yy,na.rm=T)/2,text=yarrow[1])
  }
  if(!is.na(xarrow[1])){
    arrows(y0=min(yy,na.rm=T),x0=min(xx,na.rm=T)/3*1,x1=min(xx,na.rm=T)/3*2,length=.1)
    mtext(side=1,line=-1,at=max(xx,na.rm=T)/2,text=xarrow[2])
    arrows(y0=min(yy,na.rm=T),x0=max(xx,na.rm=T)/3*1,x1=max(xx,na.rm=T)/3*2,length=.1)
    mtext(side=1,line=-1,at=min(xx,na.rm=T)/2,text=xarrow[1])
  }
  
  #browser()
  if(PointsLegend){
    levs<-c(0,.2,.4,.6,.8,1)*maxDOY;levs[1]<-levs[1]+1;levs[length(levs)]<-levs[length(levs)]-1
    # legend("topright",pch=16,title=paste("Days since 1 Nov ", focalBaseYr,sep=""),col=grey(levs/maxDOY)
    #       ,legend=paste(round(levs)," days",sep=""),box.col=NULL,bty="n")
    #points(pch=16,-4:0,rep(-0.005,length(levs)),col=rainbowcols[round(levs)])
    
    legend(legpos,pch=16,title=paste("Days since 1 Nov ", focalBaseYr,sep=""),col=rainbowcols[round(levs)]
           ,legend=paste(round(levs)," days",sep=""),box.col=NULL,bty="n")
    #legend("topright",pch=1,paste("Days since 1 Nov ", focalBaseYr,sep=""),
    #       ,legend=paste(round(levs)," days",sep=""),bty="n",text.col=NA)
    
  }
  box();abline(h=0,lty=1,col="grey");abline(v=0,lty=1,col="grey")
  return()
}

win.graph()
BivariateWinterComp(df=annual.anom.df,xvar="Te_anom",yvar="cumpre_anom",maxDOY=134,focalBaseYr=2010,
                    Focalpoints=T,PointsLegend=T,focalBag=F,
                    xlabb="Te anomaly (C)",ylabb="Cumulative precipitation anomaly (mm)"
                    ,xarrow=c("colder","warmer")
                    ,yarrow=c("dryer","wetter"))
win.graph()
BivariateWinterComp(df=annual.anom.df,xvar="smoothNDVI_anom",yvar="cumpre_anom",maxDOY=134,focalBaseYr=2010,
                    Focalpoints=T,PointsLegend=T,focalBag=F,
                    xlabb="NDVI anomaly",ylabb="Cumulative precipitation anomaly (mm)"
                    ,xarrow=c("less productive","more productive")
                    ,yarrow=c("dryer","wetter"))
win.graph()
BivariateWinterComp(df=annual.anom.df,xvar="smoothNDVI_anom",yvar="Te_anom",maxDOY=134,focalBaseYr=2010,
                    Focalpoints=T,PointsLegend=T,focalBag=F,
                    xlabb="NDVI anomaly",ylabb="Te anomaly (C)")
# win.graph()
# BivariateWinterComp(df=annual.anom.df,xvar="tmin_anom",yvar="cld_anom",maxDOY=134,focalBaseYr=2010,
#                     Focalpoints=T,PointsLegend=T,focalBag=F,
#                     xlabb="Tmin anomaly (C)",ylabb="Cloudiness anomaly (%)",
#                     xarrow=c("colder","warmer")
#                     ,yarrow=c("cloudier","clearer"))
# win.graph()
# BivariateWinterComp(df=annual.anom.df,xvar="tmin_anom",yvar="wnd_anom",maxDOY=134,focalBaseYr=2006,
#                     Focalpoints=T,PointsLegend=T,focalBag=F,
#                     xlabb="Tmin anomaly (C)",ylabb="Wind speed anomaly",legpos="topright",
#                     xarrow=c("colder","warmer")
#                     ,yarrow=c("more still","windier"))




win.graph()
BivariateWinterComp(df=annual.anom.df,xvar="smoothNDVI_anom",yvar="cumpre_anom",maxDOY=134,focalBaseYr=2009,
                    Focalpoints=T,PointsLegend=T,focalBag=F,
                    xlabb="NDVI anomaly",ylabb="Cumulative precipitation anomaly (mm)"
                    ,xarrow=c("less productive","more productive")
                    ,yarrow=c("dryer","wetter"))
# win.graph()
# BivariateWinterComp(df=annual.anom.df,xvar="tmin_anom",yvar="cld_anom",maxDOY=134,focalBaseYr=2009,
#                     Focalpoints=T,PointsLegend=T,focalBag=F,
#                     xlabb="Tmin anomaly (C)",ylabb="Cloudiness anomaly (%)",
#                     xarrow=c("colder","warmer")
#                     ,yarrow=c("cloudier","clearer"))

# win.graph()
# BivariateWinterComp(df=annual.anom.df,xvar="tmin_anom",yvar="cld_anom",maxDOY=134,focalBaseYr=2005,
#                     Focalpoints=T,PointsLegend=T,focalBag=F,
#                     xlabb="Tmin anomaly (C)",ylabb="Cloudiness anomaly (%)",
#                     xarrow=c("colder","warmer")
#                     ,yarrow=c("dryer","wetter"))
# 
# win.graph()
# BivariateWinterComp(df=annual.anom.df,xvar="tmin_anom",yvar="cld_anom",maxDOY=134,focalBaseYr=2005,
#                     Focalpoints=T,PointsLegend=T,focalBag=F,
#                     xlabb="Tmin anomaly (C)",ylabb="Cloudiness anomaly (%)",
#                     xarrow=c("colder","warmer")
#                     ,yarrow=c("cloudier","clearer"))






# plot and summary statistics of daily NDVI anomalies+++++++++++++++++++####

long.term.daily.mnNDVI <- aggregate(smoothNDVIdf,by=list(substr(smoothNDVIdf$allDate,6,10)),FUN=mean)
#convert the observations to anomalies from the long-term cell&day-level means
require(plyr)
df3 <- join(smoothNDVIdf,long.term.daily.mnNDVI,by="DOYsinceBase",type="left")
df3 <- df3[,(ncol(smoothNDVIdf)+1):ncol(df3)]
anomname <- "smoothNDVI_anom"
anom <- smoothNDVIdf[["smoothNDVI"]]-df3[["smoothNDVI"]]
smoothNDVIdf[[anomname]]<-anom













################### older code ##########

#+++++++++++++++++++ load the Tmin Torpor d.f.+++++++++++++++++++####

load(file="C:\\Data\\WHRC\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\Torpor_df.rdata")
#Tmin_torpor.dat
Tmin_torpor.dat<-Tmin_torpor.dat[!duplicated(Tmin_torpor.dat),]

#+++++++++++ test Torpor days differences between years  ++++++++++####
ss<-which((Tmin_torpor.dat$years.from.baseline >2000)&(Tmin_torpor.dat$days.sinceOct31 < 150)&(Tmin_torpor.dat$years.from.baseline <2012))
plot(Tmin_torpor.dat$years.from.baseline.fac[ss],Tmin_torpor.dat$allCFSR[ss],col="grey")
TukeyTminTorpor <- TukeyTable(classes=Tmin_torpor.dat$years.from.baseline.fac[ss],dependent=Tmin_torpor.dat$allCFSR[ss],
                              compnames=c('higher Tmin_stress','no diff.','lower Tmin_stress'))
TukeyTminTorpor

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####




# days.since.base<-function(mydates,baseline="-01-01")
# {
#   my.years<-as.numeric(substr(mydates,1,4))
#   prev.baseline <- as.Date(paste(my.years-1,baseline,sep=""))
#   nr.of.days.in.year <- as.numeric(as.Date(paste(my.years-1,"-12-31",sep=""))-as.Date(paste(my.years-1,"-01-01",sep="")))+1
#   days.since <- as.numeric(mydates-prev.baseline)
#   my.years[days.since>=nr.of.days.in.year]<-my.years[days.since>=nr.of.days.in.year]+1
#   days.since <- days.since%%nr.of.days.in.year-1
#   return(cbind(days.since,my.years))
# }
