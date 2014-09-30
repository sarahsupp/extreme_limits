# combining the extreme limits analyses into a wrapper that we can run with multiple hummingbird species
# Describes the model of (1) Arrival/catch, (2) Molt, and (3) Weight observed in Arizona and how it covaries with energetic demand and 
# resource availability in Mexican wintering grounds.

#load the libraries
library(lme4)
library(bbmle)
library(lattice)
library(latticeExtra)
library(ggplot2)

#set working directory
wd = "/Users/sarah/Documents/GitHub/extreme_limits/"
datapath = "/Users/sarah/Desktop/Dropbox/ActiveResearchProjects/Hummingbird_extreme_limits/"
setwd(wd)

#load the RData
load(paste(datapath, "yearly.catch.stat.rdata", sep=""))
load(paste(datapath, "yearly.climate.rdata", sep=""))
load(paste(datapath, "site.dat.rdata", sep=""))

#source the files that contain functions for the analysis
source('Model_evaluation_v4.r')
source('data_prep_for_transfer_PSAB_only.r')
source('Plot_conditional_effects_interaction_models.r')

#---------------------------------------------------------------------------
#                                  functions
#---------------------------------------------------------------------------
getMOLTquant <- function(quantile.){
  #Calculate quantiles of molt distribution per site & year ('Molt.quant...')
  tapply(site.dat$PriMary.Molt, INDEX=list(site.dat$LOC, site.dat$yearfac),
         FUN=quantile, type=1, probs=c(quantile.), na.rm=T)}

#make ordered categorical variables describing primary molt and fat
ordermolt <- function(x){ordered(x, levels=c("M","R",1:8,0,9,"F","L"))}
ordermolt2 <- function(x){ordered(toupper(x), levels=c("M","R",1:8,0,9,"F","L"))}
orderfat <- function(x){ordered(x, levels=c(0, "T" ,1:3))}
#---------------------------------------------------------------------------
#              generate the results for arrival, molt, and weight
#---------------------------------------------------------------------------

#visualize climate patterns for the years
plot(yearly.climate$yr, yearly.climate$winterNDVI, type = "b")
plot(yearly.climate$yr, yearly.climate$yearly.Te.10C.q, type = "b")
plot(yearly.climate$yr, yearly.climate$Tmin, type = "b")
plot(yearly.climate$yr, yearly.climate$wintercumpre, type = "b")

#----------------------- results pertaining to ARRIVAL
# Calculate quantiles of arrival distribution per site & year ('sess.quant...'). 
# quant.50 indicates the median arrival date at a given site and year.
#note that in the sess.quant variables the year is irrelevant
yearly.catch.stat$sess.quant.05 <- as.Date(yearly.catch.stat$sess.quant.05, origin="2015-01-01")
yearly.catch.stat$sess.quant.25 <- as.Date(yearly.catch.stat$sess.quant.25, origin="2015-01-01")
yearly.catch.stat$sess.quant.50 <- as.Date(yearly.catch.stat$sess.quant.50, origin="2015-01-01")
yearly.catch.stat$sess.quant.75 <- as.Date(yearly.catch.stat$sess.quant.75, origin="2015-01-01")
yearly.catch.stat$sess.quant.95 <- as.Date(yearly.catch.stat$sess.quant.95, origin="2015-01-01")

# Load the yearly climate (and NDVI) data which will serve as predictors and merge it with the 
# arrival/catch metrics in the data frame 'yearly.catch.stat' to be used for modelling
yearly.catch.stat <- merge(yearly.catch.stat, yearly.climate)

# Plot arrival responses to a proxy for physiological (yearly.Te.10C.q) demand and resrouce availability (NDVI)
xyplot(sess.quant.50 ~ yearly.Te.10C.q | LOC, data=yearly.catch.stat) + layer(panel.smoother(x, y, method = "lm"))
xyplot(sess.quant.50 ~ winterNDVI|LOC, data=yearly.catch.stat) + layer(panel.smoother(x, y, method = "lm"))

# Using aforementioned function evaluate hierarchical models of median Molt stage as a linear function of 
# annual physiological demand and resource availability, allowing for a random intercept associated with 'site'
attach(yearly.catch.stat)    #FIXME: doesn't work yet? Something is stopping the function from running
arrival.model <- mod.evaluation(yname='sess.quant.50', centering='CGM', stand=T, ordered.fac.treatment = "as.num", 
               log.D=T, R="minwinterNDVI", D="yearly.Te.10C.q", N = "n.birds") #TODO: Should be standardisation=T, instead of stand=T ??
detach(yearly.catch.stat)


#----------------------- results pertaining to MOLT
#keep only the variables you need
site.dat.molt <- site.dat[, c("LOC", "PriMary.Molt", "yearfac", "mofac")]
site.dat.molt$PriMary.Molt <- ordermolt(site.dat.molt$PriMary.Molt)

# quant.50 indicates the median molt stage at a given site and year
MOLT.quant.05 <- ordermolt(levels(site.dat.molt$PriMary.Molt)[as.vector(getMOLTquant(.05))])
MOLT.quant.25 <- ordermolt(levels(site.dat.molt$PriMary.Molt)[as.vector(getMOLTquant(.25))])
MOLT.quant.50 <- ordermolt(levels(site.dat.molt$PriMary.Molt)[as.vector(getMOLTquant(.50))])
MOLT.quant.75 <- ordermolt(levels(site.dat.molt$PriMary.Molt)[as.vector(getMOLTquant(.75))])
MOLT.quant.95 <- ordermolt(levels(site.dat.molt$PriMary.Molt)[as.vector(getMOLTquant(.95))])
MOLT.quants <- cbind.data.frame(MOLT.quant.05, MOLT.quant.25, MOLT.quant.50, MOLT.quant.75, MOLT.quant.95)

# calculate variables to hold the site names ('LOC') and years ('yr') associated with the quantiles
quant.temp <- tapply(site.dat.molt$PriMary.Molt, INDEX=list(site.dat.molt$LOC, site.dat.molt$yearfac),
                     FUN=quantile, type=1, probs=c(.05), na.rm=T)
LOC <- rownames(quant.temp)
yr <- rep(colnames(quant.temp), each=length(LOC))

# calculate the nr of birds in each sample ('n.birds')
n.birds <- as.vector(tapply(site.dat.molt$PriMary.Molt, INDEX=list(site.dat.molt$LOC, site.dat.molt$yearfac),
                            FUN=function(x){length(which(!is.na(x)))}))

#save the new metrics in the d.f. "MOLT.quants"
MOLT.quants <- cbind.data.frame(LOC, yr, n.birds, MOLT.quants)
yearly.molt.stat <- merge(MOLT.quants, yearly.climate, by='yr')

#Plot molt responses to a proxy for physiological (yearly.Te.10C.q) demand and resource availability (NDVI)
xyplot(MOLT.quant.50 ~ yearly.Te.10C.q|LOC, data=yearly.molt.stat) + layer(panel.smoother(x, y, method = "lm"))
xyplot(MOLT.quant.50 ~ winterNDVI|LOC, data=yearly.molt.stat) + layer(panel.smoother(x, y, method = "lm"))

# use mod.evaluation function evaluate hierarchical models of median Molt stage as a linear function 
# of annual physiological demand and resource availability, allowing for a random intercept associated with 'site' 
#http://www.quantpsy.org/interact/interactions.htm
attach(yearly.molt.stat)
molt.model <- mod.evaluation(yname='MOLT.quant.50', centering='CGM', stand=T, ordered.fac.treatment = "as.num", 
               log.D=T, R="minwinterNDVI", D="yearly.Te.10C.q")
detach(yearly.molt.stat)


#----------------------- results pertaining to WEIGHT
# How does weight vary with fat?
#fat classes 0 and P can be lumped
site.dat$Fat[site.dat$Fat=="P"] <- 0
site.dat$Fat <- orderfat(site.dat$Fat)
plot(site.dat$Weight ~ site.dat$Fat, col="light grey", xlab = "Fat", ylab = "Weight")

# How does weight vary with molt?
site.dat$PriMary.Molt <- ordermolt2(site.dat$PriMary.Molt)
plot(site.dat$Weight ~ site.dat$PriMary.Molt, col="light grey", xlab = "Primary Molt", ylab = "Weight")

# FYI: How are observations distributed across Molt classes?
table(site.dat$PriMary.Molt)

#Which pairs of primary molt classes differ in weight?
tt<-round(sort(TukeyHSD(aov(site.dat$Weight ~ site.dat$PriMary.Molt))[[1]][,4]), 3)
tt[tt<0.2]

#Which pairs of secondary molt classes differ in weight?
site.dat$Secondary.Molt <- ordermolt(site.dat$Secondary.Molt)
plot(site.dat$Weight ~ site.dat$Secondary.Molt, col="light grey")
table(site.dat$Secondary.Molt)
tt<-round(sort(TukeyHSD(aov(site.dat$Weight ~ site.dat$Secondary.Molt))[[1]][,4]), 3)
tt[tt<.2]

#Which pairs of body molt classes differ in weight?
site.dat$Body.Molt <- ordermolt(site.dat$Body.Molt)
plot(site.dat$Weight ~ site.dat$Body.Molt, col="light grey", xlab="Body Molt Class", ylab="Weight")
table(site.dat$Body.Molt)
tt<-round(sort(TukeyHSD(aov(site.dat$Weight ~ site.dat$Body.Molt))[[1]][,4]),3)
tt[tt<.2]

#Which pairs of gorget/head molt classes differ in weight?
site.dat$Gorget.head.molt <- ordermolt(site.dat$Gorget.head.molt)
plot(site.dat$Weight ~ site.dat$Gorget.head.molt, col="light grey",xlab="gorget head molt", ylab="Weight")
table(site.dat$Gorget.head.molt)
tt<-round(sort(TukeyHSD(aov(site.dat$Weight ~ site.dat$Gorget.head.molt))[[1]][,4]),3)
tt[tt<.2]

#Which pairs of tail molt classes differ in weight?
site.dat$Tail.Molt <- ordermolt(site.dat$Tail.Molt)
plot(site.dat$Weight ~ site.dat$Tail.Molt, col="light grey", xlab="tail molt", ylab="Weight")
table(site.dat$Tail.Molt)
tt<-round(sort(TukeyHSD(aov(site.dat$Weight ~ site.dat$Tail.Molt))[[1]][,4]),3)
tt[tt<.2]

#keep only the variables you need for the modelling analysis
site.dat <- site.dat[,c("LOC", "Weight", "yearfac", "mofac")]

#FIXME: Start Here
yearly.weight.stat <- merge(x=site.dat, y=yearly.climate, by.x='yearfac', by.y='yr')
rm(site.dat, yearly.climate)
````

Plot molt responses to a proxy for physiological (yearly.Te.10C.q) demand and resrouce availability (NDVI)
```{r fig.width=7, fig.height=6, warning=FALSE}
require(lattice)
require(latticeExtra)
xyplot(Weight ~ yearly.Te.10C.q|LOC, data=yearly.weight.stat) + layer(panel.smoother(x, y, method = "lm"))
xyplot(Weight ~ winterNDVI|LOC, data=yearly.weight.stat) + layer(panel.smoother(x, y, method = "lm"))
```

Load a function ('mod.evaluation') to evaluate hierarchical models
````{r}
source('/Users/sarah/Documents/GitHub/extreme_limits/Model_evaluation_v4.r')
````

Using aforementioned function evaluate hierarchical models of weight as a linear function of annual physiological demand and resource availability, allowing for a random intercept associated with 'site' 
````{r}
attach(yearly.weight.stat)
#http://www.quantpsy.org/interact/interactions.htm
mod.evaluation(yname='Weight', centering='CGM', stand=T, ordered.fac.treatment = "as.num", log.D=T, R="minwinterNDVI", D="yearly.Te.10C.q", N=NULL)

detach(yearly.weight.stat)
````






