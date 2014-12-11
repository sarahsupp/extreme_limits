#functions to calculate Standard Operative T for hummingbirds
#using meteo input data 
#based on Don Powers's email "Estimating Operative Temperature"

#vARIABLES TO GET FROM METEO
# Ta          ambient T (Kelvin)
# u           wind speed (m/s)
# Sp          direct shortwave radiation (W m^(-2)) 
# Sd          diffuse shortwave radiation (W m^(-2)) 
# Li          incoming longwave radition (W m^(-2)). 

#Ta: use NCEP CFSR TMP or T MIN at 2 m
#Li:Use NCEP CFSR DLWRF? Yes, it's 'at the surface' 
#Sp & Sd: Does NCEP CFSR DSWRF include the sum of both?
# check calculate_diffuse_fraction.R
#u wind speed
#NCEP var table:
#http://rda.ucar.edu/datasets/ds094.0/#metadata/grib2.html?_do=y
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####  
#caclulate aerodynamic resistance to convective heat transfer
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####  
ra.calc <- function(u #wind speed in m/s
                    ){
  # ra=aerodynamic resistance to convective heat transfer (s m-1)
  # the equation below was estimated for a small sparrow
  ra <- 37.76*(u^(-0.495))
  return(ra)
}  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate long + shortwave radiation absorbed
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
Rabs.calc <- function(Sp, #direct shortwave radiation (W m^(-2))
                      ApOVERA = 0.25, #ratio of projected shadow area to total surface area. 
                      #This is basically used to measure how much of the surface area is 
                      #directly absorbing shortwave radiation. 0.25 is a conservative starting estimate
                      Sd , #diffuse shortwave radiation. This is usually measured with a shadow radiometer
                      aS = 0.89, #mean shortwave absorptivity. 0.89 was estimated for 2 bird species
                      aL = 0.97,#mean longwave absorptivity.Walsberg 1992
                      Li #incoming longwave radiation
                      ){
  Rabs.shortwave <- ((Sp*ApOVERA) + Sd) * aS
  Rabs.longwave <-  Li * aL

  Rabs <- Rabs.shortwave + Rabs.longwave
  return(Rabs)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate operative temperature
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
Te.calc <- function(Ta, #ambient T
                    Rabs, # long-wave+shortwave radiation absorbed (W m-2)
                    sigma = 5.67e-8, #Stefan Boltman constant W m^(-2) K^(-4)
                    epsilon = 0.95, #Emissivity of the bird's surface
                    rhoCp = 1200, #product of the density and specific heat capacity of air (J m-3 K^(-1))
                    ra # aerodynamic resistance to convective heat transfer (s m-1)
                    ){
  if (any(Ta < 253)){cat("STOP !!! Ta needs to be provided in K !!!\n");
                cat("provided Ta range is: ",range(Ta,na.rm=T),"\n");browser()}
  Te <- Ta + (Rabs - sigma*epsilon*(Ta^4))/(rhoCp / (4*sigma*epsilon*(Ta^3) + ra))
  return(Te)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#calculate standard operative temperature
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
Tes.calc <- function(Te,
                     Tb = 42+273, #42 C, or 42 + 273 K
                     u # wind speed in m/s)
                    ){
  Tes <- Tb - ((1 + (0.26 * sqrt(u)))*(Tb-Te))
  return(Tes)
  #return((0.26 * sqrt(u))*(Tb-Te))
}
#test
#  Te. <-  273+c(0:40)
#  u. <- rep(1,41)
#  Tes. <- Tes.calc(Te=Te.,u=u.)
#  plot(Te.-273,Tes.-273);abline(0,1)
# 
# (1/.26)^2
# Tes. <- Tes.calc(Te=Te.,u=(1/.26)^2)
# plot(Te.-273,Tes.-273);abline(0,1)
# 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#a first "complete" function to estimate standard operative temperature
#drawing on the functions above
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
Tes.calc.compl <- function(Ta,
                           u,
                           Sp,
                           Sd,
                           Li
                          ){
  #calc aerodynamic resistance to convective heat transfer
  ra <- ra.calc(u=u)
  #calc longwave+shortwave radiation absorbed
  Rabs <- Rabs.calc(Sp=Sp, Sd=Sd, Li=Li) 
  Te <- Te.calc(Ta=Ta,ra=ra,Rabs=Rabs)
  Tes <- Tes.calc(Te,u=u)
  win.graph();plot(Ta-273,Te-273,pch=16,cex=.1,col=rgb(.1,.1,.1,.025),xlim=range(c(Ta-273,Te-273),na.rm=T),ylim=range(c(Ta-273,Te-273),na.rm=T));abline(0,1)  
  win.graph();plot(Te-273,Tes-273,pch=16,cex=.1,col=rgb(.1,.1,.1,.025),xlim=range(c(Tes-273,Te-273),na.rm=T),ylim=range(c(Tes-273,Te-273),na.rm=T));abline(0,1)  
  return(cbind.data.frame(Te,Tes))
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
# source the code to calculate diffuse and direct irradiation components
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####

#source("C:\\Users\\pbeck.WHRC\\Dropbox\\Hummers\\Hummer_code\\P4_Extreme_events_Broad_tailed\\calculate_diffuse_fraction.R")
source("C:\\Users/sarah/Documents/GitHub/extreme_limits/calculate_diffuse_fraction_code_copy.R")
#provides the function
#SpSd.calc(Rsurface,Rclearsky,solarzen)
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
#a second "complete" function to estimate standard operative temperature
#drawing on the functions above
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
Tes.calc.compl.incl.rad <- function(Ta,
                           u,
                           Li,
                           Rsurface,
                           R_extra_terr,
                           solarzen                                  
                  ){
                    SpSd <- SpSd.calc(Rsurface=Rsurface,R_extra_terr=R_extra_terr,solarzen=solarzen)
                    win.graph();par(mfrow=c(2,2))
                    hist(Rsurface);hist(R_extra_terr);hist(SpSd[,1]);hist(SpSd[,2])
                    Tes <- Tes.calc.compl(Ta=Ta,u=u,Sp=SpSd[,1],Sd=SpSd[,2],Li=Li)
                    return(Tes)
                  }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####

# an example to test functionality
#Rabs. <- Rabs.calc(Sp=20,Sd=5,Li=2)

#Tes.calc.compl(Ta.=283,u.=10,Sp.=20,Sd.=5,Li.=2)
#Tes.calc.compl.incl.rad(Ta=283,u=10,Rsurface=40,R_extra_terr=23,Li=2,solarzen=.5)

