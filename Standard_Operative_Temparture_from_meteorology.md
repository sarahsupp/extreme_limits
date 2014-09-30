#check how you work with the data for implementation example

## Calculating Standard Operative Temperature

#### Description
*Standard_Operative_Temperature_from_meteorology.r* contains functions to calculate Operative and Standard Operative temperature from standard meteorological inputs, such as those thac can be obtained from NCEP CFSR reanalysis (NCEP variable table: http://rda.ucar.edu/datasets/ds094.0/#metadata/grib2.html?_do=y).

#### Example

# an example to test functionality
#Rabs. <- Rabs.calc(Sp=20,Sd=5,Li=2)

#Tes.calc.compl(Ta.=283,u.=10,Sp.=20,Sd.=5,Li.=2)
#Tes.calc.compl.incl.rad(Ta=283,u=10,Rsurface=40,R_extra_terr=23,Li=2,solarzen=.5)


require(raster)
# load solar radiation functions
source("C:/Data/Dropbox/Hummers/Hummer_code/Hummer_repository/Diffuse_fraction_of_solar_radiation.R")

e.g of R_extra_terr.calc
#test
#tt;plot(tt);abline(h=0)


#calculate extraterrestrial radiation for a month at a Mexican latitude
```r
mydates <- as.Date(as.Date("2012-01-01"):as.Date("2012-02-01"),origin="1970-01-01")
Extra.terr.rad.Mexico <- R_extra_for_site.vec(thisdate=mydates,
                          lat.in.deg=20.123,
                          original.zone="America/Mexico_City")
head(Extra.terr.rad.Mexico)
#the four rows represent the 6 hr periods starting midnight UTC
#the colums represent days
```
#now that you know extraterrestrial radiation, you can estimate how known total shortwave irradiation #at the surface is partitioned in direct and diffuse radiation. Total irradiation at the surface can be extracted from reanlysis data such as NCEP CFSR.
```r
Surface.shortwave <- Extra.terr.rad.Mexico/rnorm(length(Extra.terr.rad.Mexico)) #dummy data
SpSd.calc()
```


#testing
SpSd.calc(20,30,.5)
IdoverI.calc(kt=.1)

########Idoveri aka kd

#### Author(s)
Pieter Beck (psabeck@gmail.com)

#### Version Log
Version | Date     | Description
--------|----------|------------
1.0     | 09-12-13 | Initial version
