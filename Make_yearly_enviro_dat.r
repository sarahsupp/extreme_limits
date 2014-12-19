# code to make the yearly climate data file from the raw data, 
# to be used in the models for the extreme limits manuscript results
# Code originally written by Pieter Beck, revised by Sarah R. Supp

#datpath <- "/Users/sarah/Documents/github/extreme_limits/data/"
datpath <- "C:/Users/tcormier/Dropbox (WHRC)/hb_extremelimits/DATA-USED-IN-EXTREME-LIMITS/TC_recreate/"
load(paste(datpath, "annual.anom.df.rdata", sep=""))

#a function for yearly climate data
yearly.climdat <- function(tapply.out, nm){
  tapply.out <-as.data.frame(tapply.out)
  tapply.out <- cbind.data.frame(tapply.out, as.numeric(rownames(tapply.out)))
  colnames(tapply.out)[2] <- "yr"
  colnames(tapply.out)[1] <- nm  
  return(tapply.out)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate mean Tes during winter and append ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
yearly.Tes <- tapply(annual.anom.df$Tes, annual.anom.df$BaseYear.x, mean)
yearly.Tes <- yearly.climdat(yearly.Tes,nm="Tes")
yearly.climate <- yearly.Tes
#yearly.catch.stat <- merge(yearly.catch.stat,yearly.Tes)
      
#       #+++++++++++++++++++++++++++++++++++++++++++++++++++++
#       #calculate mean Te during winter and append ####
#       #+++++++++++++++++++++++++++++++++++++++++++++++++++++
#       yearly.Te <- tapply(annual.anom.df$Te,annual.anom.df$BaseYear.x,mean)
#       yearly.Te <- yearly.climdat(yearly.Te,nm="Te")
#       yearly.climate <- merge(yearly.climate,yearly.Te)
#       #yearly.catch.stat <- merge(yearly.catch.stat,yearly.Tes)
 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate mean Tmin during winter and append ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
yearly.Tmin <- tapply(annual.anom.df$tmin, annual.anom.df$BaseYear.x, mean)
yearly.Tmin <- yearly.climdat(yearly.Tmin, nm="Tmin")
yearly.climate <- merge(yearly.climate, yearly.Tmin)
#yearly.catch.stat <- merge(yearly.catch.stat,yearly.Tes)

# #+++++++++++++++++++++++++++++++++++++++++++++++++++++
# #calculate day 133 nEpochs Te < 12.2 ####
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++
# winterend.allbelow10 <- annual.anom.df[annual.anom.df$DOYsinceBase.x==133,]
# winterend.allbelow10 <- tapply(winterend.allbelow10$cumallbelow10,winterend.allbelow10$BaseYear.x,mean)  
# winterend.allbelow10  <- yearly.climdat(winterend.allbelow10 ,nm="wintercumallbelow10")
# yearly.climate <- merge(yearly.climate,winterend.allbelow10)
# #yearly.catch.stat <- merge(yearly.catch.stat,winterend.sub12.2)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate day 10%, 25% and 50% quantiles of Te
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#which quantile does 10C represent?
Te.10C.quant <- function(x){ 100 * length(which(x <= 10)) / length(x) }
yearly.Te.10C.q <- tapply(annual.anom.df$Te, annual.anom.df$BaseYear.x, Te.10C.quant)
yearly.Te.10C.q <- yearly.climdat(yearly.Te.10C.q,nm = "yearly.Te.10C.q")
yearly.climate <- merge(yearly.climate, yearly.Te.10C.q)
rm(yearly.Te.10C.q)


#what's the 10% quantile?
yearly.Te.10C.quant <- tapply(annual.anom.df$Te, annual.anom.df$BaseYear.x, Te.10C.quant)

Quant10perc <- function(x){quantile(x, probs=0.10)}
yearly.Te.q10 <- tapply(annual.anom.df$Te, annual.anom.df$BaseYear.x, Quant10perc)
yearly.Te.q10 <- yearly.climdat(yearly.Te.q10, nm = "yearly.Te.q10")
yearly.climate <- merge(yearly.climate,yearly.Te.q10)
rm(yearly.Te.q10)

Quant25perc <- function(x){quantile(x, probs=0.25)}
yearly.Te.q25 <- tapply(annual.anom.df$Te, annual.anom.df$BaseYear.x, Quant25perc)
yearly.Te.q25 <- yearly.climdat(yearly.Te.q25, nm="yearly.Te.q25")
yearly.climate <- merge(yearly.climate, yearly.Te.q25)
rm(yearly.Te.q25)

Quant50perc <- function(x){quantile(x, probs=0.5)}
yearly.Te.q50 <- tapply(annual.anom.df$Te, annual.anom.df$BaseYear.x, Quant50perc)
yearly.Te.q50 <- yearly.climdat(yearly.Te.q50, nm="yearly.Te.q50")
yearly.climate <- merge(yearly.climate, yearly.Te.q50)
rm(yearly.Te.q50)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate mean NDVI and precip during winter and append ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
yearly.NDVI <- tapply(annual.anom.df$smoothNDVI, annual.anom.df$BaseYear.x, mean)
yearly.NDVI <- yearly.climdat(yearly.NDVI,nm="NDVI")
yearly.climate <- merge(yearly.climate,yearly.NDVI)
yearly._1NDVI <- yearly.NDVI
yearly._1NDVI$minNDVI <- -1*yearly._1NDVI$NDVI
yearly.climate <- merge(yearly.climate,yearly._1NDVI)

yearly.pre <- tapply(annual.anom.df$pre, annual.anom.df$BaseYear.x, mean) * 60 * 60 * 6
yearly.pre <- yearly.climdat(yearly.pre,nm="pre")
yearly.climate <- merge(yearly.climate,yearly.pre)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#calculate day 133 NDVI and cummprecip and append ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++
winterend.NDVI <- annual.anom.df[annual.anom.df$DOYsinceBase.x == 133, ]
winterend.NDVI <- tapply(winterend.NDVI$smoothNDVI, winterend.NDVI$BaseYear.x, mean)  
winterend.NDVI <- yearly.climdat(winterend.NDVI, nm = "winterNDVI")
yearly.climate <- merge(yearly.climate, winterend.NDVI)

winterend._1NDVI <- winterend.NDVI 
winterend._1NDVI$minwinterNDVI <- -1*winterend._1NDVI$winterNDVI
yearly.climate <- merge(yearly.climate, winterend._1NDVI)

winterend.cumpre <- annual.anom.df[annual.anom.df$DOYsinceBase.x == 133, ]
winterend.cumpre <- tapply(winterend.cumpre$cumpre, winterend.cumpre$BaseYear.x, mean)  
winterend.cumpre <- yearly.climdat(winterend.cumpre, nm="wintercumpre")
yearly.climate <- merge(yearly.climate, winterend.cumpre)

#the year in yearly.climate denotes the second year of that winter, rather than the
#baseyear !! to conform with the capture data from Arizona !
yearly.climate$yr <- yearly.climate$yr + 1

#save(yearly.climate, file="C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\yearly.climate.rdata")
#save(yearly.climate, file="C:\\Data\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\yearly.climate.rdata")
#save(yearly.climate, file="C:/Users/sarah/Documents/GitHub/extreme_limits/data/yearly.climate.rdata")
save(yearly.climate, file="C:/Users/tcormier/Dropbox (WHRC)/hb_extremelimits/DATA-USED-IN-EXTREME-LIMITS/TC_recreate/yearly.climate.rdata")

#load("C:\\Data\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\yearly.climate.rdata")
#load("C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\Results\\yearly.climate.rdata")
#load("C:/Users/sarah/Documents/GitHub/extreme_limits/data/yearly.climate.rdata"))
load("C:/Users/tcormier/Dropbox (WHRC)/hb_extremelimits/DATA-USED-IN-EXTREME-LIMITS/TC_recreate/yearly.climate.rdata")

pairs(yearly.climate,pch=19)

# attach(yearly.climate)
# win.graph()
# plot(wintercumallbelow10,winterNDVI,pch=NA)
# text(wintercumallbelow10,winterNDVI,yr)
# cor.test(wintercumallbelow10,winterNDVI)
# cor.test(wintercumallbelow10[yr!=2011],winterNDVI[yr!=2011])
