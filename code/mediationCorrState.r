library(mediation)
library(sensemakr)

### re-estimate effect on PerCorr for statetest dataset
datCorrState <- dat%>%filter(hasPerCorr&hasStatetest)%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))


corrAll2 <- update(corrAll,data=datCorrState)



#### does PerCorr affect statetest?

### regression estimator

posPart=function(x) ifelse(x>0,x,0)
corrStateReg <- lm(update(covForm,Scale_Score7S~Z*perCorr+.+posPart(pretestC+1)+class-virtual),
                  data=datCorrState)

corrStateSens1 <- sensemakr(model = corrStateReg,
                                treatment = "perCorr",
                           benchmark_covariates = c("pretestC",
                                                    "Scale_Score5imp",
                                                    "acceleratedTRUE","pretestMissTRUE","logTime","Z"),
                           kd=c(.5,1,1.5))
plot(corrStateSens1)
summary(corrStateSens1)

source('g:/My Drive/rcode/hhh.r')

sens=sensitivityTable(corrStateReg,X=model.frame(update(covForm,perCorr~.+Z),data=datCorrState),Y=datCorrState$Scale_Score7S,treatment="perCorr")

round(sens,3)

refMod <- update(corrStateReg,.~.-Z*perCorr+Z)

diagPlots(corrStateReg)

corrStateRegPoly <- update(corrStateReg,.~.-Z*perCorr+Z*poly(perCorr,3))
anova(corrStateRegPoly,refMod)
anova(corrStateRegPoly,corrStateReg)

diagPlots(corrStateRegPoly)
nonLinPlot(corrStateRegPoly,"perCorr")

corrStateRegSpline <- update(corrStateReg,.~.-Z*perCorr+Z*splines::ns(perCorr,3))

diagPlots(corrStateRegSpline)
nonLinPlot(corrStateRegSpline,"perCorr")

anova(corrStateRegSpline,refMod)
anova(corrStateRegSpline,corrStateReg)


## GPS
## gps <- SuperLearner(datCorrState$perWorked,model.frame(update(covForm,Z~.+class),data=datCorrState),
##                     SL.library=c('SL.bayesglm','SL.glmnet','SL.gam','SL.ranger'))


## qqnorm(datCorrState$perWorked-gps$SL.predict)
## qqline(datCorrState$perWorked-gps$SL.predict)
## hist(datCorrState$perWorked-gps$SL.predict)hist(datCorrState$perWorked-gps$SL.predict)
## mean(datCorrState$perWorked-gps$SL.predict)
## mean((datCorrState$perWorked-gps$SL.predict)^2)
## 1-mean((datCorrState$perWorked-gps$SL.predict)^2)/var(datCorrState$perWorked)

gpsLM <- lm(update(covForm,perCorr~Z+.),data=datCorrState)

## datCorrState$gps <- gps$SL.predict
## datCorrState$gps2 <- dnorm(datCorrState$perWorked,datCorrState$gps,sd(datCorrState$perWorked-datCorrState$gps))
## lm_robust(pretestC~perWorked,data=datCorrState,fixed_effects=class)
## lm_robust(pretestC~perWorked+gps,data=datCorrState,fixed_effects=class)
## lm_robust(pretestC~perWorked+gps2,data=datCorrState,fixed_effects=class)
## lm_robust(pretestC~perWorked+fitted(gpsLM),data=datCorrState,fixed_effects=class)

datCorrState$gps <- fitted(gpsLM)
pvals <- NULL
covs <- model.matrix(update(covForm,.~.+Z-virtual),data=datCorrState)
for(k in 2:ncol(covs)){
  cc <- scale(covs[,k])
  mod0 <- lm_robust(cc~perCorr,
                   data=datCorrState,fixed_effects=class)
  mod <- lm_robust(cc~perCorr+gps,
                   data=datCorrState,fixed_effects=class)
  pvals[colnames(covs)[k]] <- mod$p.value['perCorr']
  eff[colnames(covs)[k]] <- mod$coef['perCorr']
}

corrStateRegPolyGPS <- update(corrStateRegPoly,.~.+gps+I(gps^2)+I(gps^3))
anova(corrStateRegPolyGPS,corrStateRegPoly)
anova(corrStateRegPolyGPS,refMod)
anova(corrStateRegPolyGPS,update(corrStateRegPolyGPS,.~.-Z:poly(perCorr,3)))

diagPlots(corrStateRegPolyGPS)
nonLinPlot(corrStateRegPolyGPS,"perCorr")


med <- mediate(model.m=corrAll2,model.y=corrStateReg,treat="Z",mediator="perCorr",data=datCorrState)

summary(med)

sens <- medsens(med)

summary(sens)

#### both mediators
### refit perwork model
est2med <- function(datStar,returnModels=FALSE,prog=TRUE){
  if(prog) cat('.')
  workAll3 <- update(workAll2,data=datStar)
  corrAll3 <- update(corrAll2,data=datStar)

  corrWorkState <- update(corrStateReg,.~.+perWorked-perCorr:Z,data=datStar)

  acme <-  coef(workAll3)["Z"]*coef(corrWorkState)['perWorked']+
    coef(corrAll3)["Z"]*coef(corrWorkState)['perCorr']
  acde <- coef(corrWorkState)['Z']
  tot <- acme+acde

  est=c(acme=acme,acde=acde,tot=tot)

  if(returnModels) return(list(est=est,corrWorkState=corrWorkState,workAll3=workAll3,corrAll3=corrAll3))

  est
}

corrWorkStateMed <- list(
  est=est2med(datCorrState),
  bs = replicate(5000,
                est2med(
                  datCorrState[
                    sample(1:nrow(datCorrState),
                           nrow(datCorrState),
                           replace=TRUE),]))
)

par(mfrow=c(1,3))
if(nrow(corrWorkStateMed$bs)==3) apply(corrWorkStateMed$bs,1,hist)
par(mfrow=c(1,1))

with(corrWorkStateMed,
     cbind(estimate=est,
           bias=rowMeans(bs)-est,
           se=apply(bs,1,sd),
           T=est/apply(bs,1,sd),
           p=2*pnorm(-abs(est/apply(bs,1,sd)))))


save(med,sens,corrWorkStateMed,file="results/mediationStateCorr.RData")
