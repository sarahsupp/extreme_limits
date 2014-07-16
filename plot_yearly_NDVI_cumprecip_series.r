load("annual.anom.df.rdata")

#########################################
# left panel
#########################################

precip.fac <- 6*60*60 ######## I can't trace where this scaling of the precip comes from, please double-check if the plotted values make sense!

maxDOY<-max(annual.anom.df$DOYsinceBase.y)

rainbowcols<-rainbow(maxDOY*5/4)[1:(maxDOY+1)]

win.graph(w=6,h=6)

attach(annual.anom.df)
plot(cumpre*precip.fac,smoothNDVI.x,ylim=range(smoothNDVI.x)*c(.97,1),
     xlab="Cumulative precipitation [mm]",ylab="Normalized Difference Vegetation Index"
     ,pch=16,col=rainbowcols[DOYsinceBase.y+1],cex=.75)

yearly.end.NDVI.vals <- NULL

labs<-c("'00-'01","'01-'02","'02-'03","'03-'04","'04-'05","'05-'06","'06-'07","'07-'08","'08-'09","'09-'10","'10-'11","'11-'12")
aa <- 0
for (a in unique(annual.anom.df$BaseYear.y))
{
  aa <- aa+1
  lines(precip.fac*cumpre[BaseYear.y==a],smoothNDVI.x[BaseYear.y==a],col="dark grey")
  ss <- (BaseYear.y==a)&(DOYsinceBase.y==133)
  
  #add labels for particular years
  if (is.element(aa,c(10,11))){text(precip.fac*cumpre[ss]-14,smoothNDVI.x[ss]-0.007,labs[aa],
                                    cex=1.05,font=2) }
  end.NDVI.val <- smoothNDVI.x[ss]
  yearly.end.NDVI.vals <- c(yearly.end.NDVI.vals,end.NDVI.val)
}

detach(annual.anom.df)

dats<-cbind.data.frame(c(1+14,30+14,61+14,92+14,120+13),c("15 Nov","15 Dec","15 Jan","15 Feb","15 March"))
legend("topright",pch=16, col=rainbowcols[dats[,1]]
       ,legend=dats[,2],box.col=NULL,bty="n")

#########################################
# right panel
#########################################

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#winterNDVI versus El Nino (Oceanic Nino Index)
#using DJF values taken from
#http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
ONI.djf <- cbind(c(2001:2012),
                 c(-0.7,-.2,1.1,.3,.6,-.9,.7,-1.5,-.8,1.6,-1.4,-.9))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####

win.graph(w=3,h=6)
plot(ONI.djf[,2],yearly.end.NDVI.vals,col=0,pch=0,xlab="Oceanic Nino Index\nfor Dec, Jan, Feb",
     axes=F,ylab="",ylim=c(.55,.71));box()
segments(ONI.djf[,2],yearly.end.NDVI.vals,yearly.end.NDVI.vals*0,col=ifelse(ONI.djf[,2]>0,"red","blue"),lwd=3)
abline(v=0)
axis(3)

#########################################
# 
#########################################

#write each panel away to your favourite format
#and join to a single figure in editing software