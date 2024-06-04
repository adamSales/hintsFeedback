library(mediation)
library(sensemakr)
#library(SuperLearner)
#library(tmle)
#### does perWorked affect statetest?

### regression estimator
datState <- dat%>%filter(hasStatetest)%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))



posPart=function(x) ifelse(x>0,x,0)
workStateReg <- lm(update(covForm,Scale_Score7S~Z*perWorked+.+posPart(pretestC+1)+class-virtual),
                  data=datState)

refMod <- update(workStateReg,.~.-Z*perWorked+Z)

diagPlots(workStateReg)

workStateSens1 <- sensemakr(model = workStateReg,
                                treatment = "perWorked",
                           benchmark_covariates = c("pretestC",
                                                    "Scale_Score5imp",
                                                    "acceleratedTRUE","pretestMissTRUE","logTime","Z"),
                           kd=c(.5,1,1.5))


workStateRegPoly <- lm(update(covForm,Scale_Score7S~Z*poly(perWorked,3,raw=TRUE)+.+posPart(pretestC+1)+class-virtual),
                      data=datState)
anova(workStateRegPoly,refMod)
anova(workStateRegPoly,workStateReg)

diagPlots(workStateRegPoly)
nonLinPlot(workStateRegPoly,"perWorked")
#ggsave('perWorkEffectState.jpg')

workStateRegSpline <- lm(update(covForm,Scale_Score7S~Z*splines::ns(perWorked,3)+.+posPart(pretestC+1)+class-virtual),
                      data=datState)

diagPlots(workStateRegSpline)
nonLinPlot(workStateRegSpline,"perWorked")

anova(workStateRegSpline,refMod)
anova(workStateRegSpline,workStateReg)


## GPS
## gps <- SuperLearner(datState$perWorked,model.frame(update(covForm,Z~.+class),data=datState),
##                     SL.library=c('SL.bayesglm','SL.glmnet','SL.gam','SL.ranger'))


## qqnorm(datPost$perWorked-gps$SL.predict)
## qqline(datPost$perWorked-gps$SL.predict)
## hist(datPost$perWorked-gps$SL.predict)hist(datPost$perWorked-gps$SL.predict)
## mean(datPost$perWorked-gps$SL.predict)
## mean((datPost$perWorked-gps$SL.predict)^2)
## 1-mean((datPost$perWorked-gps$SL.predict)^2)/var(datPost$perWorked)

gpsLM <- lm(update(covForm,perWorked~Z+.+class
                   #.-pre_MA_total_scoreimp+splines::ns(pre_MA_total_scoreimp,3)#+
#                             I(pre_MSE_total_scoreimp^2)+I(pre_MSE_total_scoreimp^3)
                   ),data=datState)

## datPost$gps <- gps$SL.predict
## datPost$gps2 <- dnorm(datPost$perWorked,datPost$gps,sd(datPost$perWorked-datPost$gps))
## lm_robust(pretestC~perWorked,data=datPost,fixed_effects=class)
## lm_robust(pretestC~perWorked+gps,data=datPost,fixed_effects=class)
## lm_robust(pretestC~perWorked+gps2,data=datPost,fixed_effects=class)
## lm_robust(pretestC~perWorked+fitted(gpsLM),data=datPost,fixed_effects=class)

datState$gps <- fitted(gpsLM)
pvals <- pvals0 <- NULL
eff <- eff0 <- NULL
covs <- model.matrix(update(covForm,.~.+Z-virtual),data=datState)
for(k in 2:ncol(covs)){
  cc <- scale(covs[,k])
  mod0 <- lm_robust(cc~perWorked,
                   data=datState,fixed_effects=class)
  mod <- lm_robust(cc~perWorked+gps,
                   data=datState,fixed_effects=class)
  pvals0[colnames(covs)[k]] <- mod0$p.value['perWorked']
  eff0[colnames(covs)[k]] <- mod0$coef['perWorked']
  pvals[colnames(covs)[k]] <- mod$p.value['perWorked']
  eff[colnames(covs)[k]] <- mod$coef['perWorked']
}

round(cbind(eff0,pvals0,eff,pvals),3)

workStateRegPolyGPS <- update(workStateRegPoly,.~.+gps+I(gps^2)+I(gps^3))
anova(workStateRegPolyGPS,workStateRegPoly)
anova(workStateRegPolyGPS,refMod)
anova(workStateRegPolyGPS,update(workStateRegPolyGPS,.~.-Z:poly(perWorked,3)))

diagPlots(workStateRegPolyGPS)
nonLinPlot(workStateRegPolyGPS,"perWorked")

workMod <- update(workAll2,data=datState)
save(workStateReg,workStateSens1,workStateRegPoly,workStateRegSpline,workMod,file="results/workStateMods.RData")

med <- mediate(model.m=update(workAll2,data=datState),model.y=workStateReg,treat="Z",mediator="perWorked",data=datState)

sens <- medsens(med)

summary(sens)

save(med,sens,file="results/mediationPerWorkedState.RData")
