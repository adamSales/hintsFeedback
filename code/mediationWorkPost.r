library(mediation)
library(SuperLearner)
library(tmle)
#### does perWorked affect posttest?

### regression estimator

posPart=function(x) ifelse(x>0,x,0)
workPostReg <- lm(update(covForm,postS~Z*perWorked+.+posPart(pretestC+1)+class-virtual),
                  data=datPost)

workPostSens1 <- sensemakr(model = workPostReg,
                                treatment = "perWorked",
                           benchmark_covariates = c("pretestC",
                                                    "Scale_Score5imp",
                                                    "acceleratedTRUE","pretestMissTRUE","logTime","Z"),
                           kd=c(.5,1,1.5))
plot(workPostSens1)

summary(workPostSens1)

refMod <- update(workPostReg,.~.-Z*perWorked+Z)

diagPlots(workPostReg)

workPostRegPoly <- lm(update(covForm,postS~Z*poly(perWorked,3)+.+posPart(pretestC+1)+class-virtual),
                      data=datPost)
anova(workPostRegPoly,refMod)
anova(workPostRegPoly,workPostReg)

diagPlots(workPostRegPoly)
nonLinPlot(workPostRegPoly,"perWorked")

workPostRegSpline <- lm(update(covForm,postS~Z*splines::ns(perWorked,3)+.+posPart(pretestC+1)+class-virtual),
                      data=datPost)

diagPlots(workPostRegSpline)
nonLinPlot(workPostRegSpline,"perWorked")

anova(workPostRegSpline,refMod)
anova(workPostRegSpline,workPostReg)


## GPS
## gps <- SuperLearner(datPost$perWorked,model.frame(update(covForm,Z~.+class),data=datPost),
##                     SL.library=c('SL.bayesglm','SL.glmnet','SL.gam','SL.ranger'))


## qqnorm(datPost$perWorked-gps$SL.predict)
## qqline(datPost$perWorked-gps$SL.predict)
## hist(datPost$perWorked-gps$SL.predict)hist(datPost$perWorked-gps$SL.predict)
## mean(datPost$perWorked-gps$SL.predict)
## mean((datPost$perWorked-gps$SL.predict)^2)
## 1-mean((datPost$perWorked-gps$SL.predict)^2)/var(datPost$perWorked)

gpsLM <- lm(update(covForm,perWorked~Z+.),data=datPost)

## datPost$gps <- gps$SL.predict
## datPost$gps2 <- dnorm(datPost$perWorked,datPost$gps,sd(datPost$perWorked-datPost$gps))
## lm_robust(pretestC~perWorked,data=datPost,fixed_effects=class)
## lm_robust(pretestC~perWorked+gps,data=datPost,fixed_effects=class)
## lm_robust(pretestC~perWorked+gps2,data=datPost,fixed_effects=class)
## lm_robust(pretestC~perWorked+fitted(gpsLM),data=datPost,fixed_effects=class)

datPost$gps <- fitted(gpsLM)
pvals <- NULL
covs <- model.matrix(update(covForm,.~.+Z-virtual),data=datPost)
for(k in 2:ncol(covs)){
  cc <- scale(covs[,k])
  mod0 <- lm_robust(cc~perWorked,
                   data=datPost,fixed_effects=class)
  mod <- lm_robust(cc~perWorked+gps,
                   data=datPost,fixed_effects=class)
  pvals[colnames(covs)[k]] <- mod$p.value['perWorked']
  eff[colnames(covs)[k]] <- mod$coef['perWorked']
}

workPostRegPolyGPS <- update(workPostRegPoly,.~.+gps+I(gps^2)+I(gps^3))
anova(workPostRegPolyGPS,workPostRegPoly)
anova(workPostRegPolyGPS,refMod)
anova(workPostRegPolyGPS,update(workPostRegPolyGPS,.~.-Z:poly(perWorked,3)))

diagPlots(workPostRegPolyGPS)
nonLinPlot(workPostRegPolyGPS,"perWorked")


med <- mediate(model.m=update(workAll2,data=datPost),model.y=workPostReg,treat="Z",mediator="perWorked",data=datPost)

sens <- medsens(med)

summary(sens)

save(med,sens,file="results/mediationPerWorked.RData")
