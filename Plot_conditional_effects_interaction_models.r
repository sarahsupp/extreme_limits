
#a function to plot the effect of x1 on y, in  a 2-level regeression model
#that includes an interaction between x1 and x2, and random varying intercept

#adapted from http://quantpsy.org/interact/hlm2.htm
#http://quantpsy.org/interact/hlm2.htm
#http://www.unc.edu/~curran/pdfs/Bauer%26Curran(2005).pdf
#http://www.amstat.org/publications/jse/v19n3/afshartous.pdf

Plot_conditional_effects_interaction_models <- function(
  alpha       = 0.05,
z1=-2,  #supply lower bound for x2 here
z2=2 ,  #supply upper bound for x2 here
mymod=mod.DR ,
switch.vars = F,
ylim.=NULL,
plott=T){
  
#Variable names in the model
  vnames <- names(fixef(mymod))[2:3];if(switch.vars){vnames <- names(fixef(mymod))[3:2]}
  ylab. <- paste("Effect of ",vnames[1]," on B",sep="")
  xlab. <- paste(vnames[2]," [s.d.]",sep="")

  #Histograms of the variables
  if(plott){hist(unique(mymod@frame[[vnames[2]]]),col="light grey",main="",xlab=xlab.)}
  
  #Parameters in the models
  #========================================================
  Intercept   = fixef(mymod)[1]
  x1_Slope    = fixef(mymod)[ifelse(switch.vars,3,2)]
  x2_Slope    = fixef(mymod)[ifelse(switch.vars,2,3)]
  x1x2_Slope  = fixef(mymod)[4]
  

#Asymptotic (Co)variances
#========================================================
vcov. <- vcov(mymod)

var_g00 <- vcov.[1,1]# can be pulled from covariance matrix or SE^2
var_g10 <- vcov.[2,2];if(switch.vars){var_g10 <- vcov.[3,3]}#0.00254722
var_g20 <- vcov.[3,3];if(switch.vars){var_g20 <- vcov.[2,2]}#0.00163216
var_g30 <- vcov.[4,4]#0.00116759
cov_g00_g20 <- vcov.[1,3];if(switch.vars){cov_g00_g20 <- vcov.[1,2]}#0.00093961
cov_g10_g30 <- vcov.[2,4];if(switch.vars){cov_g10_g30 <- vcov.[3,4]}#0.00134905

z <- seq(z1,z2,length=1000)
fz <- c(z,rev(z))
y1 <- (x1_Slope+x1x2_Slope*z)+(1.9602*sqrt(var_g10+(2*z*cov_g10_g30)+((z^2)*var_g30)))
y2 <- (x1_Slope+x1x2_Slope*z)-(1.9602*sqrt(var_g10+(2*z*cov_g10_g30)+((z^2)*var_g30)))
fy <- c(y1,rev(y2))
fline <- (x1_Slope+x1x2_Slope*z)

if (plott){plot(fz,fy,type='p',pch='.',col=0,xlab=xlab.,ylab=ylab.,ylim=ylim.)}
f0 <- array(0,c(1000))

  if (plott){polygon(x=c(fz),y=c(fy),
          col="light grey",border=NA)
  lines(z,fline,lwd=2)
  
  lines(z,f0,lty=2)}
 
return(range(c(f0,fline,fy)))
}
