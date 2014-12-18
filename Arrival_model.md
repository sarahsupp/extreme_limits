Model Arrival Variation
========================================================

This document describes the model of Arrival/catch observed in Arizona and how it covaries with energetic demand and resource availability in Mexican wintering grounds


```r
# load the Hummingbird arrival data; i.e. the d.f. yearly.catch.stat
load("C:/Users/sarah/Dropbox/ActiveResearchProjects/Hummingbird_extreme_limits/yearly.catch.stat.rdata")
```



```r
# Calculate quantiles of arrival distribution per site & year
# ('sess.quant...'). quant.50 indicates the median arrival date at a given
# site and year.
yearly.catch.stat$sess.quant.05 <- as.Date(yearly.catch.stat$sess.quant.05, 
    origin = "2015-01-01")
yearly.catch.stat$sess.quant.25 <- as.Date(yearly.catch.stat$sess.quant.25, 
    origin = "2015-01-01")
yearly.catch.stat$sess.quant.50 <- as.Date(yearly.catch.stat$sess.quant.50, 
    origin = "2015-01-01")
yearly.catch.stat$sess.quant.75 <- as.Date(yearly.catch.stat$sess.quant.75, 
    origin = "2015-01-01")
yearly.catch.stat$sess.quant.95 <- as.Date(yearly.catch.stat$sess.quant.95, 
    origin = "2015-01-01")
# note that in the sess.quant variables the year is irrelevant
```


Load the yearly climate (and NDVI) data which will serve as predictors and merge it with the arrival/catch metrics in the data frame 'yearly.catch.stat' to be used for modelling

```r
load("C:/Users/sarah/Dropbox/ActiveResearchProjects/Hummingbird_extreme_limits/yearly.climate.rdata")
# gives you yearly.climate
yearly.catch.stat <- merge(yearly.catch.stat, yearly.climate)
rm(yearly.climate)
```


Plot arrival responses to a proxy for physiological (yearly.Te.10C.q) demand and resrouce availability (NDVI)

```r
require(lattice)
```

```
## Loading required package: lattice
```

```r
require(latticeExtra)
```

```
## Loading required package: latticeExtra
## Loading required package: RColorBrewer
```

```r
xyplot(sess.quant.50 ~ yearly.Te.10C.q | LOC, data = yearly.catch.stat) + layer(panel.smoother(x, 
    y, method = "lm"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
xyplot(sess.quant.50 ~ winterNDVI | LOC, data = yearly.catch.stat) + layer(panel.smoother(x, 
    y, method = "lm"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 


Load a function ('mod.evaluation') to evaluate hierarchical models

```r
source("/Users/sarah/Documents/GitHub/extreme_limits/Model_evaluation_v4.r")
```

```
## Loading required package: lme4
```

```
## Warning: package 'lme4' was built under R version 3.0.3
```

```
## Loading required package: Matrix
```

```
## Warning: package 'Matrix' was built under R version 3.0.3
```

```
## Loading required package: Rcpp
```

```
## Warning: package 'Rcpp' was built under R version 3.0.3
```

```
## Loading required package: bbmle
```

```
## Warning: package 'bbmle' was built under R version 3.0.3
```

```
## Loading required package: stats4
```


Using aforementioned function evaluate hierarchical models of median arrival as a linear function of annual physiological demand and resource availability, allowing for a random intercept associated with 'site' 

```r
attach(yearly.catch.stat)
# http://www.quantpsy.org/interact/interactions.htm
mod.evaluation(yname = "sess.quant.50", centering = "CGM", stand = T, log.D = T, 
    R = "winterNDVI")
```

```
## samples will be weighted by 'n.birds' in model calibration
## DEMAND variable will be log-transformed
## 
## Correlation between D and R (after transform and or subset):
## 
## 	Pearson's product-moment correlation
## 
## data:  D and R
## t = -3.159, df = 97, p-value = 0.002112
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.4742 -0.1149
## sample estimates:
##     cor 
## -0.3054 
## 
## 
## Predictor variables were centered at the grand mean (CGM)
## 
## Predictor variables were standardised using grand sd
```

```
## Warning: Model failed to converge with max|grad| = 0.00209901 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.00403024 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.00211345 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.00399115 (tol = 0.002)
```

```
## Conditional effects of predictors in full model, i.e. including R*D interaction
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```
## 100*R2 for linear model mod.DR.lm : 
## 35 
## 100*R2 for linear model mod.D_R.lm : 
## 37 
## 100*R2 for linear model mod.D.lm : 
## 28 
## 100*R2 for linear model mod.R.lm : 
## 36 
## 100*R2 for linear model mod.0.lm : 
## 27 
## 
## AIC of best model: 903.9 
## 
## Model evaluation completed and tabulated:
```

```
##         mod.name dAICc.lib dAICCc.con df A.weight D_low D_high  R_low
## mod.DR    mod.DR       0.0        0.0  6        1 -3.19   1.73 -11.69
## mod.D_R  mod.D_R       3.4        1.3  5        0 -3.86   0.92 -11.84
## mod.R      mod.R       4.9        1.2  4        0    NA     NA -11.21
## mod.D      mod.D      44.0       40.3  4        0 -1.47   4.21     NA
## mod.0      mod.0      45.3       40.5  3        0    NA     NA     NA
##         R_high DR_low DR_high D_est R_est DR_est
## mod.DR   -6.70   0.03    3.88 -0.73 -9.19   1.96
## mod.D_R  -6.77     NA      NA -1.47 -9.30     NA
## mod.R    -6.39     NA      NA    NA -8.80     NA
## mod.D       NA     NA      NA  1.37    NA     NA
## mod.0       NA     NA      NA    NA    NA     NA
```

```r
detach(yearly.catch.stat)
```


Using  function evaluate hierarchical models of 25th percentile arrival as a linear function of annual physiological demand and resource availability, allowing for a random intercept associated with 'site' 

```r
attach(yearly.catch.stat)
# http://www.quantpsy.org/interact/interactions.htm
mod.evaluation(yname = "sess.quant.25", centering = "CGM", stand = T, log.D = T, 
    R = "winterNDVI")
```

```
## samples will be weighted by 'n.birds' in model calibration
## DEMAND variable will be log-transformed
## 
## Correlation between D and R (after transform and or subset):
## 
## 	Pearson's product-moment correlation
## 
## data:  D and R
## t = -3.159, df = 97, p-value = 0.002112
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.4742 -0.1149
## sample estimates:
##     cor 
## -0.3054 
## 
## 
## Predictor variables were centered at the grand mean (CGM)
## 
## Predictor variables were standardised using grand sd
```

```
## Warning: Model failed to converge with max|grad| = 0.00295662 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.0028867 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.003614 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.00298182 (tol = 0.002)
## Warning: Model failed to converge with max|grad| = 0.00361989 (tol = 0.002)
```

```
## Conditional effects of predictors in full model, i.e. including R*D interaction
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```
## 100*R2 for linear model mod.DR.lm : 
## 33 
## 100*R2 for linear model mod.D_R.lm : 
## 33 
## 100*R2 for linear model mod.D.lm : 
## 27 
## 100*R2 for linear model mod.R.lm : 
## 33 
## 100*R2 for linear model mod.0.lm : 
## 25 
## 
## AIC of best model: 891.2 
## 
## Model evaluation completed and tabulated:
```

```
##         mod.name dAICc.lib dAICCc.con df A.weight D_low D_high R_low
## mod.R      mod.R       0.0        0.0  4     0.48    NA     NA -6.77
## mod.D_R  mod.D_R       0.1        1.6  5     0.40 -1.99   2.43 -6.83
## mod.DR    mod.DR       0.7        4.3  6     0.12 -2.15   2.51 -6.85
## mod.D      mod.D      13.1       13.1  4     0.00 -0.66   3.82    NA
## mod.0      mod.0      14.9       13.8  3     0.00    NA     NA    NA
##         R_high DR_low DR_high D_est R_est DR_est
## mod.R    -2.34     NA      NA    NA -4.55     NA
## mod.D_R  -2.13     NA      NA  0.22 -4.48     NA
## mod.DR   -2.12  -1.93    1.73  0.18 -4.48   -0.1
## mod.D       NA     NA      NA  1.58    NA     NA
## mod.0       NA     NA      NA    NA    NA     NA
```

```r
detach(yearly.catch.stat)
```

