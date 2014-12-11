#lower-level code for working with the climate data for the Extreme Limits paper (PAB, TAC, SRS)


#  RUN ON ARCTIC
require(maptools)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#load the site data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#setwd('C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\R_data')
#setwd('C:\\Data\\WHRC\\Hummers\\R_data\\')
setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
load("Mexico_CFSR_level2.rdata")
objs <- ls()
rm(list=objs[!is.element(objs,c("xtractCFSRatSITES","site.vec","list.rasters"))])
rm(objs)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Extract the Tmin data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CFSR.Tmin_6hr.dir <- 'C:\\forest\\data\\cfsr\\tif\\tmin_6hr_mex'
Tmin.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Tmin_6hr.dir,site.vec,start.date="2000-01-01",end.date="2015-01-03",Varname="tmin",xtrachar=4)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Extract the Tmax data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CFSR.Tmax_6hr.dir <- 'C:\\forest\\data\\cfsr\\tif\\tmax_6hr_mex'
Tmax.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Tmax_6hr.dir,site.vec,start.date="2000-01-01",end.date="2015-01-03",Varname="tmax",xtrachar=4)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Extract the wind data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CFSR.Wnd_6hr.dir <- 'C:\\forest\\data\\cfsr\\tif\\wnd_6hr_mex'
Wnd.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Wnd_6hr.dir,site.vec,start.date="2000-01-01",end.date="2012-12-01",Varname="wnd",xtrachar=0)
#Wnd.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Wnd_6hr.dir,site.vec,start.date="2012-12-01",end.date="2015-10-03",Varname="wnd",xtrachar=0)
Wnd.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Wnd_6hr.dir,site.vec,start.date="2000-01-01",end.date="2012-06-01",Varname="wnd",xtrachar=0)

#When set to 2015-01-03 in Jan 2013 it crashed after 97 %
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Extract the DLWRF data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CFSR.Dlwrf_6hr.dir <- 'C:\\forest\\data\\cfsr\\tif\\dlwrf_6hr_mex'
Dlwrf.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Dlwrf_6hr.dir,site.vec,start.date="2000-01-01",end.date="2015-10-03",Varname="dlwrf",xtrachar=0)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Extract the DSWRF data ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CFSR.Dswrf_6hr.dir <- 'C:\\forest\\data\\cfsr\\tif\\dswrf_6hr_mex'
Dswrf.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Dswrf_6hr.dir,site.vec,start.date="2000-01-01",end.date="2015-10-03",Varname="dswrf",xtrachar=0)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Extract the precip data #### not needed for SOT - but ueful
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CFSR.Pre_6hr.dir <- 'C:\\forest\\data\\cfsr\\tif\\cam_Pre_6hr'
Pre.dat <- xtractCFSRatSITES(CFSRdir=CFSR.Pre_6hr.dir,site.vec,start.date="2000-01-01",end.date="2015-10-03",Varname="cam_pre",xtrachar=0)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#get the coordinates to get R_extra_terr for
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
setwd('C:\\forest\\data\\cfsr\\tif\\tmin_6hr_mex')
site.cellnrs <- cellFromXY(raster(
  list.rasters(  'C:\\forest\\data\\cfsr\\tif\\tmin_6hr_mex',".tif")[1]),xy=site.vec)
site.vec.coords <- cbind.data.frame(site.vec,site.cellnrs)
rm(site.cellnrs)
site.vec.coords <- site.vec.coords[,c('LATITUDE','LONGITUDE','site.cellnrs')]
site.vec.coords <- site.vec.coords[-which(duplicated(site.vec.coords$site.cellnrs)),]
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#setwd('C:\\Data\\WHRC\\Hummers\\R_data\\')
setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
save.image("Mexico_SOT_level1.rdata")

setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
load("Mexico_SOT_level1.rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#load the functions to caclulate SOT
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#source("C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\calculate_SOT_from_Met.R")
#source("C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\calculate_SOT_from_Met.R")
source("C:\\Users/sarah/Documents/GitHub/extreme_limits/calculate_SOT_from_Met.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate R_extra_for all sites and days-epochs
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

mydates<-as.Date(min(Tmin.dat$imdate):max(Tmin.dat$imdate),origin="1970-01-01")
mydates<- rep(mydates,each=nrow(site.vec.coords))
mydates<-as.data.frame(mydates)
site.vec.coords.alldates <- cbind.data.frame(site.vec.coords,mydates)
rm(site.vec.coords)
tt.<-R_extra_for_site.vec(thisdate=site.vec.coords.alldates$mydates,
                          lat.in.deg=site.vec.coords.alldates$LATITUDE,
                          original.zone="America/Mexico_City")
tt.<- cbind(as.vector(t(tt.)),1:4)
colnames(tt.)<-c('R_extra','imEpoch')
site.vec.coords.alldates <- cbind.data.frame(site.vec.coords.alldates[rep(1:nrow(site.vec.coords.alldates),each=4),],tt.)
rm(tt.)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate  solarzen for all sites and days-epochs
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#get matching 6-hourly solar elevations
zencoords <- site.vec.coords.alldates[,c('LONGITUDE','LATITUDE')]
#make mydates 6-hourly set to the middle of the epochs
myepochs <- site.vec.coords.alldates$mydates
myepochs <- as.POSIXct(myepochs)
myepochs <- myepochs+((site.vec.coords.alldates$imEpoch-1)*6+3)*60*60
solarzen <- solarzen.calc(coords=as.matrix(zencoords),datetimePOSIXct=myepochs)
site.vec.coords.alldates <- cbind.data.frame(site.vec.coords.alldates,solarzen)
rm(imEpoch, LATITUDE, LONGITUDE, R_extra, solarzen)

#test                         
#attach(site.vec.coords.alldates)
#plot(solarzen[ss],R_extra[ss],pch=16,col=rgb(.5,.5,.5,.1),cex=.2)                        
#detach(site.vec.coords.alldates)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#check for that solarzen and R_extra make sense
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
attach(site.vec.coords.alldates)
#plot(R_extra,solarzen,pch=16,cex=.1,col=rgb(0.1,0.1,0.1,0.1));abline(0,1)
plot(R_extra,solarzen,pch=16,cex=.1,col=imEpoch)
abline(0,1)
detach(site.vec.coords.alldates)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#create a joint data-set to feed to Tes.calc.compl.incl.rad
#merge with Tmin.dat, Wnd.dat,Dswrf.dat,Dlwrf.dat
clean.imEpoch <- function(x){x <- x%%4 ; x[x==0] <- 4;return(x)}

Tmin.dat$imEpoch <- clean.imEpoch(Tmin.dat$imEpoch)
Tmax.dat$imEpoch <- clean.imEpoch(Tmax.dat$imEpoch)
Wnd.dat$imEpoch <- clean.imEpoch(Wnd.dat$imEpoch)
Dlwrf.dat$imEpoch <- clean.imEpoch(Dlwrf.dat$imEpoch)
Dswrf.dat$imEpoch <- clean.imEpoch(Dswrf.dat$imEpoch)
Pre.dat$imEpoch <- clean.imEpoch(Pre.dat$imEpoch)

allmet <- merge(Tmin.dat,Wnd.dat); rm(Wnd.dat,Tmin.dat)
allmet <- merge(allmet,Tmax.dat); rm(Tmax.dat)
allmet <- merge(allmet,Dlwrf.dat); rm(Dlwrf.dat)
allmet <- merge(allmet,Dswrf.dat); rm(Dswrf.dat)
allmet <- merge(allmet,Pre.dat); rm(Pre.dat)

colnames(site.vec.coords.alldates)[c(3,4)] <- c('cellnrs','imdate')
allmet <- merge(site.vec.coords.alldates,allmet)
rm(site.vec.coords.alldates)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
allmet$tmax <- allmet$tmax-273 #convert tmax to Celsius (tmin is already in Celsius)
#setwd('C:\\Data\\WHRC\\Hummers\\R_data\\')
setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
save.image("Mexico_SOT_level2.rdata")

setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
load("Mexico_SOT_level2.rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#check for that dswrf < R_extra in all cases
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
attach(allmet)
plot(dswrf,dlwrf,pch=16,cex=.1,xlim=range(c(dswrf,dlwrf),na.rm=T),ylim=range(c(dswrf,dlwrf),na.rm=T),col=imEpoch);abline(0,1)
win.graph();par(mfrow=c(1,2))
plot(dswrf,R_extra,pch=16,cex=.1,xlim=range(c(R_extra,dswrf),na.rm=T),ylim=range(c(R_extra,dswrf),na.rm=T),col=imEpoch);abline(0,1)

win.graph();plot(dswrf,R_extra,pch=16,cex=.1,xlim=range(c(R_extra,dswrf),na.rm=T),ylim=range(c(R_extra,dswrf),na.rm=T),col=rgb(.1,.1,.1,.025));abline(0,1,lwd=2);abline(0,2)
win.graph();plot(dswrf,R_extra,pch=16,cex=.1,xlim=range(c(R_extra,dswrf),na.rm=T),ylim=range(c(R_extra,dswrf),na.rm=T),col=imEpoch);abline(0,1,lwd=2);abline(0,2)
plot(dswrf,dlwrf,pch=16,cex=.1,xlim=range(c(dlwrf,dswrf),na.rm=T),ylim=range(c(dlwrf,dswrf),na.rm=T),col=imEpoch);abline(0,1,lwd=2);abline(0,2)

#plot(solarzen,R_extra,pch=16,cex=.1,col=imEpoch)
plot(solarzen,dswrf,pch=16,cex=.1,col=imEpoch);abline(0,1)
detach(allmet)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate Tes
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#source("C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\Calculate_SOT_from_Met.R")
#source("C:/Users/sarah/Documents/GitHub/extreme_limits/Calculate_SOT_from_Met.R") #code from PAB computer
source("C:/Users/sarah/Documents/GitHub/extreme_limits/Standard_Operative_Temparture_from_meteorology.R") #code PAB sent us
attach(allmet)
undebug(Tes.calc.compl.incl.rad) #only need to debug if you are working in "debug" mode
undebug(SpSd.calc) #only need to debug if you are working in "debug" mode
Tes.dat <- Tes.calc.compl.incl.rad(Ta=tmax+273,u=wnd,Li=dlwrf,Rsurface=dswrf,R_extra_terr=R_extra,solarzen=solarzen)
#back to Celsius
Tes.dat <- Tes.dat - 273
detach(allmet)
allmet<-cbind.data.frame(allmet,Tes.dat)
rm(Tes.dat)

#setwd('C:\\Data\\WHRC\\Hummers\\R_data\\')
setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
save.image("Mexico_SOT_level3.rdata")




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#How does Tes-Tmin change with meteorology?
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++ !!!!!!!!!!! CONTINUE HERE !!!!!!!!!!! ++++++++++                          
#+++++++++++++++++++++++ !!!!!!!!!!! CONTINUE HERE !!!!!!!!!!! ++++++++++
#+++++++++++++++++++++++ !!!!!!!!!!! CONTINUE HERE !!!!!!!!!!! ++++++++++
setwd('C:\\Share\\pbeck\\Hummer_NASA\\Code_copy\\')
load("Mexico_SOT_level3.rdata")

attach(allmet)
win.graph();par(mfrow=c(2,2))
for (i in 1:4){
  ss <- imEpoch==i
  plot(tmin[ss],Tes[ss],xlim=range(c(tmin,Tes),na.rm=T),ylim=range(c(tmin,Tes),na.rm=T),
       pch=16,cex=.3,col=rgb(.1,.1,.1,.025),main=paste("imEpoch",i))
  
  abline(0,1,col=2,lwd=2);abline(10,1,col=3);abline(20,1,col=3);abline(30,1,col=3)
  abline(-10,1,col=3);abline(-20,1,col=3);abline(-30,1,col=3)
  abline(h=0,col=4);abline(v=0,col=4)
}
win.graph();par(mfrow=c(2,2))
for (i in 1:4){
  ss <- imEpoch==i
  plot(tmin[ss],Te[ss],xlim=range(c(tmin,Te),na.rm=T),ylim=range(c(tmin,Te),na.rm=T),
       pch=16,cex=.3,col=rgb(.1,.1,.1,.025),main=paste("imEpoch",i))
  
  abline(0,1,col=2,lwd=2);abline(10,1,col=3);abline(20,1,col=3);abline(30,1,col=3);
  abline(-10,1,col=3);abline(-20,1,col=3);abline(-30,1,col=3)
  abline(h=0,col=4);abline(v=0,col=4)
}
win.graph();par(mfrow=c(2,2))
for (i in 1:4){
  ss <- imEpoch==i
  plot(Te[ss],Tes[ss],xlim=range(c(Te,Tes),na.rm=T),ylim=range(c(Te,Tes),na.rm=T),
       pch=16,cex=.3,col=rgb(.1,.1,.1,.025),main=paste("imEpoch",i))
  
  abline(0,1,col=2,lwd=2);abline(10,1,col=3);abline(20,1,col=3);abline(30,1,col=3)
  abline(-10,1,col=3);abline(-20,1,col=3);abline(-30,1,col=3)
  abline(h=0,col=4);abline(v=0,col=4)
}

win.graph();par(mfrow=c(2,2))
plot(I(Tes-tmin)~wnd,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))
plot(I(Tes-tmin)~as.factor(imEpoch))
plot(I(Tes-tmin)~dswrf,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))
plot(I(Tes-tmin)~dlwrf,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))

win.graph();par(mfrow=c(2,2))
plot(I(Tes-Te)~wnd,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))
plot(I(Tes-Te)~as.factor(imEpoch))
plot(I(Tes-Te)~dswrf,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))
plot(I(Tes-Te)~dlwrf,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))

win.graph();par(mfrow=c(2,2))
plot(I(Te-tmin)~wnd,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))
plot(I(Te-tmin)~as.factor(imEpoch))
plot(I(Te-tmin)~dswrf,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))
plot(I(Te-tmin)~dlwrf,pch=16,cex=.1,col=rgb(.1,.1,.1,.025))

detach(allmet)
