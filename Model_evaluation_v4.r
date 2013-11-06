# code for evaluating mixed models and quantile regresstion for the "extreme limits" hummingbird paper
# modified from P.A. Beck

# import packages
require(lme4)
require(bbmle)

# source data and r files
source('/Users/sarah/Documents/GitHub/extreme_limits/Plot_conditional_effects_interaction_models.r')


mod.evaluation <- function(yname, D = "yearly.Te.10C.q", R = "NDVI", N = "n.birds",
                           ordered.fac.treatment = c("as.num", "as.binomial"),
                           centering = c('none', 'CGM', 'CWC'), log.D = F, log.R = F,
                           standardisation = F, year2excl = NA){
  #aim: evaluation of mixed models and quantile regression for Extreme limits" Hummer project. 
  #The model to be calibrateed has the response variable (yname) and 2 predictors (D) and (R) and its interactions.
  #This model is then compared to all its sub-models.
  #All models contain random (intercept-)effect associated with site/location 'LOC'
  #
  #Arguments
  #yname: name of the response variable (e.g. as named in an attached data frame)
  #D:     name of the predictor variable representing physiological demand (e.g. minimum annal temperature)
  #R:     name of the predictor variable representing resource availability (e.g. NDVI)
  #N:     a variable by which observations are to be weighted in the model (e.g. the nr of birds sampled)
  #ordered.fac.treatment:
  #       if the response variable is an ordered factor, how should it be treated? 
  #       Options are "as.num" which treats the response variable as if it were numeric, and
  #       "as.binomial" which treat it as a bernouilli experiment.
  #centering: Should the predictor variable be centered. Options are no centering "none", "GCM" which centers
  #       the data around the grand mean, and "CWC" which centers the data around group means (both sensu EndersTofighi2007PhsychologicalMethods)
  #       Also see: http://mlrv.ua.edu/2009/vol35_1/Robinson_Schumacke_rproof.pdf
  #log.D: Should the 'D' predictor variable be log-transformed?
  #log.R: Should the 'D' predictor variable be log-transformed?
  #standardisation: should predictor variables be standardized (i.e. divided by ther standard deviation)
  #year2excl: specify any years that need to be excluded from the model
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # get the variables
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  nbirds <- NULL
  if(!is.null(N)){
    nbirds <- get(N)
    cat("samples will be weighted by 'n.birds' in model calibration\n")}
  else{
    cat("all samples will be equally weighted calibration\n")} 
  
  D <- get(D)
  R <- get(R)
  mod.name <- c('mod.DR', 'mod.D', 'mod.R', 'mod.0', 'mod.D_R')
  y <- get(yname)  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # do you want a particular year excluded?
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  if (!is.na(year2excl)){cat("Excluding year(s): ",year2excl,"\n")
                         y<-y[yr!=year2excl]
                         D<-D[yr!=year2excl]
                         R<-R[yr!=year2excl]
                         LOC.<-LOC[yr!=year2excl]
                         nbirds. <- nbirds[yr!=year2excl]}else{LOC. <- LOC ; nbirds. <- nbirds}
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # do you want any of the predictors to be log transformed?
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (log.D){
    cat("DEMAND variable will be log-transformed\n") 
    D <- log(D) + 1}
  if (log.R){
    cat("RESOURCE variable will be log-transformed\n") 
    R <- log(R) + 1}
  cat("\nCorrelation between D and R (after transform and or subset):\n")
  print(cor.test(D, R))
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # is the y-variable an ordered factor?
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if ((is.factor(y[1])) & (ordered.fac.treatment[1] == c("as.num"))){
    cat(yname, " is an ordered factor. options are to treat as numeric or in a binomial fashion.\n")    
    cat(yname, " will be converted to numeric \n")
    y <- as.numeric(y)}
  
  if ((is.factor(y[1])) & (ordered.fac.treatment[1] == c("as.binomial"))){
    cat("\n", yname, " is an ordered factor. options are to treat as numeric or in a binomial fashion !!!\n")
    cat(yname, " is an ordered factor and will be converted to a count table !!!\n")
    y <- ordered.var.2.sim.count.table(y)}
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # apply centering and standardization ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  CGM <- function(x){(x-mean(x, na.rm=T))}#/sd(x)}
  CWC <- function(x,LOC){mns.x <- tapply(x, LOC, mean, na.rm=T)
                         mns.x <- mns.x[match(LOC, names(mns.x))]
                         mns.x <- x - mns.x
                         return(mns.x)}  
  if (centering == 'CGM'){
    D <- CGM(D)
    R <- CGM(R) 
    cat('\nPredictor variables were centered at the grand mean (CGM)\n') }
  if (centering == 'CWC'){
    D <- CWC(x=D,LOC=LOC.)
    R <- CWC(x=R,LOC=LOC.)
    cat('\nPredictor variables were centered within clusters (CWC)\n') 
  }
  
  if(standardisation){
    cat('\nPredictor variables were standardised using grand sd\n')
    D <- D/sd(D, na.rm=T)
    R <- R/sd(R, na.rm=T)
  }
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # plot the predictor variables
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  win.graph(w=9, h=4)
  par(mfrow=c(1,3))
  hist(unique(D), col="light grey")
  hist(unique(R),col="light grey")
  plot(R,D,pch=16)
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # calibrate the main model and it's sub-models
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
  if ((ordered.fac.treatment[1] == c("as.binomial")) & (!is.null(dim(y)))){
    #for models with ordinal response variables treated as binomial counts  
    mod.DR <- glmer(formula = y ~ D*R + (1|LOC.), weights=nbirds., family="binomial")
    mod.D_R <- glmer(formula = y ~ D + R + (1|LOC.), weights=nbirds., family="binomial")
    mod.D <- glmer(formula = y ~ D + (1|LOC.), weights=nbirds., family="binomial")
    mod.R <- glmer(formula = y ~ R + (1|LOC.), weights=nbirds., family="binomial")
    mod.0 <- glmer(formula = y ~ 1 + (1|LOC.), weights=nbirds., family="binomial")
    }
  else{
    #for all other models
    mod.DR <- lmer(formula = y ~ D*R + (1|LOC.), weights=nbirds.)
    mod.D_R <- lmer(formula = y ~ D + R + (1|LOC.), weights=nbirds.)
    mod.D <- lmer(formula = y ~ D + (1|LOC.), weights=nbirds.)
    mod.R <- lmer(formula = y ~ R + (1|LOC.), weights=nbirds.)
    mod.0 <- lmer(formula = y ~ 1 + (1|LOC.), weights=nbirds.)
  }  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #in the D*R model, under what conditions are the D (R) terms negative?
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  cat("Conditional effects of predictors in full model, i.e. including R*D interaction\n")
  win.graph(w=8,h=8)
  par(mfcol=c(2,2))
  
  r1 <- Plot_conditional_effects_interaction_models(mymod=mod.DR, plott=F)
  r2 <- Plot_conditional_effects_interaction_models(mymod=mod.DR, switch.vars = T, plott=F)
  
  r12 <- c(-1,1)*max(abs(c(r1,r2)), na.rm=T)
    r1 <- Plot_conditional_effects_interaction_models(mymod=mod.DR, ylim.=r12)
  r2 <- Plot_conditional_effects_interaction_models(mymod=mod.DR,ylim.=r12, switch.vars = T)
    
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # get the AIC  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #since 'n' is not defined in a mixed model, calculate a liberal AIC (mod.AIC.CON)
  #assuing n is the nr of data points (nobs.HI), and a liberal AIC (mod.AIC.LIB) assuming n is the nr of groupd (nobs.LO)
  nobs.HI <- nrow(mod.DR@frame)
  nobs.LO <- nlevels(mod.DR@frame$LOC)
  
  mod.AIC.LIB <- ICtab(mod.DR, mod.D, mod.R, mod.0, mod.D_R, sort=F, type="AICc", nobs=nobs.HI)
  mod.AIC.CON <- ICtab(mod.DR, mod.D, mod.R, mod.0, mod.D_R, sort=F, type="AICc", nobs=nobs.LO)
  
  mod.AIC <- cbind.data.frame(round(mod.AIC.LIB$dAICc,1), round(mod.AIC.CON$dAICc, 1), mod.AIC.LIB$df)
  colnames(mod.AIC) <- c('dAICc.lib', 'dAICCc.con', 'df')
  rownames(mod.AIC) <- c('mod.DR', 'mod.D', 'mod.R', 'mod.0', 'mod.D_R')
  
  #calculate Akaike weights sensu http://www.planta.cn/forum/files_planta/2006_nobbs_ecology_153.pdf
  Akaike.weight <- function(AIC.vec){
    AIC.vec <- AIC.vec-min(AIC.vec,na.rm=T)
    Akai.weight.denom <- sum(exp(-2*AIC.vec))
    Akai.weight <- (exp(-2*AIC.vec)) / Akai.weight.denom
    return(Akai.weight)
  }
  
  A.weight <- round(Akaike.weight(mod.AIC$dAICc.lib), 2)
  mod.AIC <- cbind.data.frame(mod.AIC, A.weight)
  
  conf.int95.DR <-  sqrt(diag(vcov(mod.DR)))[-1]
  conf.int95.D <-  sqrt(diag(vcov(mod.D)))[-1]
  conf.int95.R <-  sqrt(diag(vcov(mod.R)))[-1]
  conf.int95.D_R <-  sqrt(diag(vcov(mod.D_R)))[-1]
  
  #get the parameter estimates
  par.es.DR <- fixef(mod.DR)[-1]
  par.es.D <- fixef(mod.D)[-1]
  par.es.R <- fixef(mod.R)[-1]
  par.es.D_R <- fixef(mod.D_R)[-1]
  
  #get the confidence intervals for the parameter estimates
  get.conf.int <- function(p.est,SE){
    rep(p.est, each=2) + rep(SE, each=2) * c(-1.96, 1.96)}
  
  conf.int95.DR <-  get.conf.int(par.es.DR, conf.int95.DR)
  conf.int95.D <-  get.conf.int(par.es.D, conf.int95.D)
  conf.int95.R <-  get.conf.int(par.es.R, conf.int95.R)
  conf.int95.D_R <-  get.conf.int(par.es.D_R, conf.int95.D_R)
  
  conf.int95.D <- c(conf.int95.D, rep(NA, 4))
  conf.int95.R <- c(NA, NA, conf.int95.R, NA, NA)
  conf.int95.0 <- rep(NA, 6) 
  conf.int95.D_R <- c(conf.int95.D_R,rep(NA, 2))
  
  conf.int95 <- rbind(conf.int95.DR,conf.int95.D, conf.int95.R, conf.int95.0, conf.int95.D_R)                
  conf.int95 <- round(conf.int95, 2)
  conf.int95[which(conf.int95 >= 100) ] <- round(conf.int95[which(conf.int95 >= 100 )])
  colnames(conf.int95) <- c('D_low', 'D_high', 'R_low', 'R_high', 'DR_low', 'DR_high')
  
  mod.eval <- cbind.data.frame(mod.name, mod.AIC, conf.int95)
  mod.eval <- mod.eval[with(mod.eval, order(mod.AIC$dAICc.lib)), ]
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # print the R2 of predicted and observed for the equivalent linear models
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if (!((ordered.fac.treatment[1] == c("as.binomial")) & (!is.null(dim(y))))){
    #for all other models
    mod.DR.lm <- lm(formula = as.numeric(y) ~ D*R + LOC., weights=nbirds.)
    mod.D_R.lm <- lm(formula = as.numeric(y) ~ D + R + LOC., weights=nbirds.)
    mod.D.lm <- lm(formula = as.numeric(y) ~ D + LOC., weights=nbirds.)
    mod.R.lm <- lm(formula = as.numeric(y) ~ R + LOC., weights=nbirds.)
    mod.0.lm <- lm(formula = as.numeric(y) ~ LOC., weights=nbirds.)
    print.r2.lm <- function(modname){
      cat("100*R2 for linear model",modname,": \n")
      cat((round(100*cor(as.numeric(y[!is.na(y)]), as.numeric(get(modname)$fitted.value))^2)), "\n")
      return()
    } 
    print.r2.lm("mod.DR.lm")
    print.r2.lm("mod.D_R.lm")
    print.r2.lm("mod.D.lm")
    print.r2.lm("mod.R.lm")
    print.r2.lm("mod.0.lm")
  }  

  #print the AIC of the best model
  cat("\nAIC of best model:", AIC(get(rownames(mod.eval)[1])), "\n")
  
  cat("\nModel evaluation completed and tabulated:\n")
  return(mod.eval)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   #convert an ordered categorical variable to a simulated count table ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ordered.var.2.sim.count.table<-function(x){
  #This code drops unused factor levels at the start and end of the rank
  #that way you are guaranteed that the count table runs from c(0 max), to c(max 0)
  #which I'd think is desirable if the data are to be considered binomial
  x.num <- as.numeric(x)
  x.num.crop <- x.num-min(x.num, na.rm=T)  
  x.num.crop.tab <- cbind(x.num.crop, max(x.num.crop, na.rm=T) - x.num.crop)
  
  return(x.num.crop.tab)
}
