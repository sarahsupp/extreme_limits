#based on Lanini2010.pdf page 16

#Id is diffuse radiation
#I is 'global radiation' (DSWRF?) (for a horizontal plane, global irradiance is the sum of the diffuse and the direct component)
#Id/I (IdoverI) is the fraction of diffuse radiation in global radiation 
#IdoverI is estimated as a function of kt (Liu and Jordan 1960) (and in some cases solar zenith angle)

#this function implements the Reindl* method (Helbig 2009)

#kt is the clearness index
#The clearness index measures the proportion of horizontal extraterrestrial radiation (Io)
#reaching the surface.
#it is defined as kt = I / (Io cos solarzen)
#From NCEP CFSR it might defined as DSWRF/CSDSF 

#where CSDSF = Clear sky downward solar flux  !!!!! is this equal to horiz. extraterrestrial rad?
#NOT SURE! instead use the extrat function in the sirad package

#and DSWRF = Downward short wave radiation flux 

#solarzen is the solar zenith in radians

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate the fraction of diffuse radiation in global radiation
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
IdoverI.calc <- function(kt,solarzen) #Idoveri aka kd
  {
  IdoverI <- kt*NA
  Under.78 <- which(kt < 0.78)
  if (length(Under.78) > 0){  IdoverI[Under.78] <- 1.4 - 1.749*kt[Under.78] + 0.177*sin(pi/2 - solarzen[Under.78])}

  Under.3 <- which(kt <= 0.3)
  if (length(Under.3) > 0 ){  IdoverI[Under.3] <- 0.1020 - 0.248*kt[Under.3]}
  
  
  Over.78 <- which(kt >= 0.78)
  if (length(Over.78) > 0){  IdoverI[Over.78] <- 0.147}
#   if (kt <= 0.3) {IdoverI <- 0.1020 - 0.248*kt}else{
#     if(kt < 0.78){
#       solar.elev <- pi/2 - solarzen
#       IdoverI <- 1.4 - 1.749*kt + 0.177*sin(solar.elev)}else{
#       IdoverI <- 0.147}}
  return(IdoverI)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate direct (Sp) and diffuse (Sd) radiation from global radiation
#and horizontal extraterrestrial radiation 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
SpSd.calc <- function(Rsurface,R_extra_terr,solarzen)
  {
  if (max(solarzen,na.rm=T) > 2*pi){cat("STOP STOP STOP provide solarzenith in radiance to SpSd.calc\n");browser()}
  kt <- Rsurface/R_extra_terr
  #the formula in Lanini 2010 p1 is
  #kt <- Rsurface/(Io*cos(solarzen))
  #but this is for Io*cos(solarzen) being the horizontal extraterrestrial radiation
  #and thus is adjusted for latitude/solarzen before comparison with Rsurface
  #I believe sirad:extrat provides latitude-corrected (ie horizontal) extraterrestrial radiation
  IdoverI <- IdoverI.calc(kt,solarzen)
  IdoverI[IdoverI < 0] <- 0 ; IdoverI[IdoverI > 1] <- 1
  SpSd <- Rsurface * cbind(1-IdoverI,IdoverI)
  #SpSd is NA (only?) if kt is NA. kt is NA (only?) when R_extra_terr is NA (ie at night)
  SpSd[is.na(SpSd)]<-0
  win.graph();plot(SpSd[,2],SpSd[,1],pch=16,cex=.1,col=rgb(.1,.1,.1,.025),xlab="Diffuse irrad. (Sp)",ylab="Direct irrad. (Sd)");abline(0,1)
  return(SpSd)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate solar zenith based on lat, lon & time of day
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
solarzen.calc <- function(coords,datetimePOSIXct)
  {
  require(maptools)
  solarelv <- solarpos(crds=coords,dateTime=datetimePOSIXct)
  solarelv <- solarelv[,2]
  #convert to radians
  solarelv <- pi*solarelv/180
  #convert to solar zenith (0 when sun is overhead) in radians
  solarzen <- pi/2 - solarelv
  #set below-horizon zeniths to pi/2
  solarzen[solarzen > pi/2] <- pi/2
  cat("in zolar zenith values, pi/2 (90degs) represents horizon/sub-horizon\n
      while 0 represents directly over-head\n")
  return(solarzen)  
}

  #test
  #load("Mexico_CFSR_level1.rdata")
  #  objs <- ls();rm(list=objs[!is.element(objs,c("test","site.vec"))]);rm(objs)
  # date.time <- as.POSIXct(test$imdate)+(test$imEpoch-1)*60*60*6
  # solarelv <- solarzen.calc(coords=site.vec@coords[1:100,],datetimePOSIXct=date.time[1:100])

#elevation is in degrees and measured from the horizon
#plot(date.time[1:100],test$tmin[1:100],pch=ifelse(solarelv>0,1,16),cex=(solarelv+90)/30)
#text(x=date.time[1:100],y=test$tmin[1:100],round(solarelv))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate hourly extraterrestrial irradiance in W/m2 using sirad package
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
R_extra_terr.calc<-function(thisdate,lat.in.deg)
  {
  require(sirad)
  JulianDay <- sirad::dayOfYear(thisdate)#dayOfYear("2011-01-01")
  lat.in.rad <- radians(lat.in.deg)
  R_extra_terr <- extrat(i=JulianDay,lat.in.rad)#[[2]]
  #extrat[[1]] uses MJ/(day m^2) units
  #extrat[[2]] uses MJ/(hr m^2) units
  #convert output to W/m2
  conv.fac.daily <- (1000000/ 86400)
  conv.fac.hourly <- (1000000/ 86400)*24
  R_extra_terr[[1]] <- conv.fac.daily *  R_extra_terr[[1]]
  R_extra_terr[[2]] <- conv.fac.hourly *  R_extra_terr[[2]]
  #set negative hourly extrat irrad to 0
  #R_exta_terr <- R_exta_terr[[2]]
  R_extra_terr[[2]][R_extra_terr[[2]]<0]<-0
  R_extra_terr <- R_extra_terr[[2]]
  R_extra_terr <- matrix(R_extra_terr,ncol=24)
  cat("calculated 24hrs of extraterrestrial irradiance for\n"
      ,nrow(R_extra_terr)," latitude-date combinations\n")
  #output is hourly starting at solar midning
  return(R_extra_terr)
}
#test
#tt<-R_extra_terr.calc(thisdate=c("2012-01-19","2012-01-19"),lat.in.deg=c(60,-69))
#tt;plot(tt);abline(h=0)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#shift an hourly vector starting at midnight to start at midnight UTC
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
shift_to_UTC <- function(hourly_tser,original.zone)
  {
  #http://stackoverflow.com/questions/6828937/what-to-do-with-imperfect-but-useful-functions
  # Shift a vector over by n spots
  # wrap adds the entry at the beginning to the end
  # pad does nothing unless wrap is false, in which case it specifies whether to pad with NAs
  shift.vec <- function(vec,n,wrap=TRUE,pad=FALSE) {
    if(length(vec)<abs(n)) { 
      #stop("Length of vector must be greater than the magnitude of n \n") 
    }
    if(n==0) { 
      return(vec) 
    } else if(length(vec)==n) { 
      # return empty
      length(vec) <- 0
      return(vec)
    } else if(n>0) {
      returnvec <- vec[seq(n+1,length(vec) )]
      if(wrap) {
        returnvec <- c(returnvec,vec[seq(n)])
      } else if(pad) {
        returnvec <- c(returnvec,rep(NA,n))
      }
    } else if(n<0) {
      returnvec <- vec[seq(1,length(vec)-abs(n))]
      if(wrap) {
        returnvec <- c( vec[seq(length(vec)-abs(n)+1,length(vec))], returnvec )
      } else if(pad) {
        returnvec <- c( rep(NA,abs(n)), returnvec )
      }
      
    }
    return(returnvec)
  }
  require(timeDate)
  tt1<-timeDate("2010-01-01 00:00:00",zone=original.zone)
  tt2<-timeDate("2010-01-01 00:00:00",zone="UTC")
  shift.by <- as.numeric(tt2-tt1)
  cat("the 24 hour series will be shifted earlier hrs by ",shift.by," hours to match UTC\n")
  if (is.matrix(hourly_tser)){
    shifted_tser<-t(apply(hourly_tser,1,shift.vec,n=shift.by))
    }else{shifted_tser <- shift.vec(hourly_tser,n=shift.by)}
  return(shifted_tser)
}
#test
#tt <- shift_to_UTC(hourly_tser=1:24,original.zone="America/Mexico_City")
#ttshift <- shift_to_UTC(hourly_tser=tt,original.zone="America/Mexico_City")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#given a date, latitude, UTC epoch, and time zone, calculate R_extra
#for that latitude, and UTC epoch
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
R_extra_for_site.vec <- function(thisdate,lat.in.deg,original.zone)
  {
  if(length(thisdate)!=length(lat.in.deg)){
    cat("Please provide thisdate and lat.in.deg of equal length\n");browser()}
  #get the 24 R_extra_terr values for this lat & date
  R_extra_terr_24 <- R_extra_terr.calc(thisdate=thisdate,lat.in.deg=lat.in.deg)
  #determine by how many hours the series need to be shifted
  R_extra_terr_24UTC <- shift_to_UTC(R_extra_terr_24,original.zone=original.zone)
  #convert the R_extra_terr values from 24 hour to epoch
  epochmn <- function(x){tapply(x,rep(1:4,each=6),mean)}
  R_extra_terr_epochUTC <- t(apply(R_extra_terr_24UTC,1,epochmn))
  cat("all 24 hour series of extra-terrestrial R converted to 4 6-hour UTC epochs\n")
  rm(R_extra_terr_24,R_extra_terr_24UTC)
  return(R_extra_terr_epochUTC)
}
#mydates<-as.Date(as.Date("2012-01-01"):as.Date("2012-02-01"))
#tt.<-R_extra_for_site.vec(thisdate=mydates,
#                          lat.in.deg=rep(site.vec.coords$LATITUDE[1],length(mydates)),
#                          original.zone="America/Mexico_City")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####


#testing
#SpSd.calc(20,30,.5)
#IdoverI.calc(kt=.1)

#site.vec.coords
