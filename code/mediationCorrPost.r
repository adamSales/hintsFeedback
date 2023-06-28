library(mediation)
library(sensemakr)

### re-estimate effect on PerCorr for posttest dataset
datCorrPost <- dat%>%filter(hasPerCorr&hasPosttest)%>%group_by(
                                                        class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))


corrAll2 <- update(corrAll,data=datCorrPost)



#### does PerCorr affect posttest?

### regression estimator

posPart=function(x) ifelse(x>0,x,0)
corrPostReg <- lm(update(covForm,postS~Z*perCorr+.+posPart(pretestC+1)+class-virtual),
                  data=datCorrPost)

corrPostSens1 <- sensemakr(model = corrPostReg,
                                treatment = "perCorr",
                           benchmark_covariates = c("pretestC",
                                                    "Scale_Score5imp",
                                                    "acceleratedTRUE","pretestMissTRUE","logTime","Z"),
                           kd=c(.5,1,1.5))
plot(corrPostSens1)

source('g:/My Drive/rcode/hhh.r')

sens=sensitivityTable(corrPostReg,X=model.frame(update(covForm,perCorr~.+Z),data=datCorrPost),Y=datCorrPost$postS,treatment="perCorr")

summary(corrPostSens1)

refMod <- update(corrPostReg,.~.-Z*perCorr+Z)

#diagPlots(corrPostReg)

corrPostRegPoly <- update(corrPostReg,.~.-Z*perCorr+Z*poly(perCorr,3))
anova(corrPostRegPoly,refMod)
anova(corrPostRegPoly,corrPostReg)

#diagPlots(corrPostRegPoly)
nonLinPlot(corrPostRegPoly,"perCorr")
ggsave('results/corrPostPolynomial.jpg')

corrPostRegSpline <- update(corrPostReg,.~.-Z*perCorr+Z*splines::ns(perCorr,3))

#diagPlots(corrPostRegSpline)
nonLinPlot(corrPostRegSpline,"perCorr")

anova(corrPostRegSpline,refMod)
anova(corrPostRegSpline,corrPostReg)

corrPostRegSpline4 <- update(corrPostReg,.~.-Z*perCorr+Z*splines::ns(perCorr,4))
anova(corrPostRegSpline4,corrPostRegSpline)


## GPS
## gps <- SuperLearner(datPost$perWorked,model.frame(update(covForm,Z~.+class),data=datPost),
##                     SL.library=c('SL.bayesglm','SL.glmnet','SL.gam','SL.ranger'))


## qqnorm(datPost$perWorked-gps$SL.predict)
## qqline(datPost$perWorked-gps$SL.predict)
## hist(datPost$perWorked-gps$SL.predict)hist(datPost$perWorked-gps$SL.predict)
## mean(datPost$perWorked-gps$SL.predict)
## mean((datPost$perWorked-gps$SL.predict)^2)
## 1-mean((datPost$perWorked-gps$SL.predict)^2)/var(datPost$perWorked)

gpsLM <- lm(update(covForm,perCorr~Z+.),data=datPost)

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
  mod0 <- lm_robust(cc~perCorr,
                   data=datPost,fixed_effects=class)
  mod <- lm_robust(cc~perCorr+gps,
                   data=datPost,fixed_effects=class)
  pvals[colnames(covs)[k]] <- mod$p.value['perCorr']
  eff[colnames(covs)[k]] <- mod$coef['perCorr']
}

corrPostRegPolyGPS <- update(corrPostRegPoly,.~.+gps+I(gps^2)+I(gps^3))
anova(corrPostRegPolyGPS,corrPostRegPoly)
anova(corrPostRegPolyGPS,refMod)
anova(corrPostRegPolyGPS,update(corrPostRegPolyGPS,.~.-Z:poly(perCorr,3)))

#diagPlots(corrPostRegPolyGPS)
nonLinPlot(corrPostRegPolyGPS,"perCorr")

save(corrAll2,corrPostReg,corrPostSens1,sens,corrPostRegPoly,corrPostRegSpline,file='results/corPostModels.RData')

med <- mediate(model.m=update(corrAll2,data=datPost),model.y=corrPostReg,treat="Z",mediator="perCorr",data=datPost)

summary(med)

sens <- medsens(med)

summary(sens)



#### both mediators
### refit perwork model
est2med <- function(datStar,returnModels=FALSE,prog=TRUE){
  if(prog) cat('.')
  workAll3 <- update(workAll2,data=datStar)
  corrAll3 <- update(corrAll2,data=datStar)

  corrWorkPost <- update(corrPostReg,.~.+perWorked-perCorr:Z,data=datStar)

  acme <-  coef(workAll3)["Z"]*coef(corrWorkPost)['perWorked']+
    coef(corrAll3)["Z"]*coef(corrWorkPost)['perCorr']
  acde <- coef(corrWorkPost)['Z']
  tot <- acme+acde

  est=c(acme=acme,acde=acde,tot=tot)

  if(returnModels) return(list(est=est,corrWorkPost=corrWorkPost,workAll3=workAll3,corrAll3=corrAll3))

  est
}

corrWorkPostMed <- list(
  est=est2med(datCorrPost),
  bs = replicate(5000,
                est2med(
                  datCorrPost[
                    sample(1:nrow(datCorrPost),
                           nrow(datCorrPost),
                           replace=TRUE),]))
)

par(mfrow=c(1,3))
if(nrow(corrWorkPostMed$bs)==3) apply(corrWorkPostMed$bs,1,hist)
par(mfrow=c(1,1))

with(corrWorkPostMed,
     cbind(estimate=est,
           bias=rowMeans(bs)-est,
           se=apply(bs,1,sd),
           T=est/apply(bs,1,sd),
           p=2*pnorm(-abs(est/apply(bs,1,sd)))))


save(med,sens,corrWorkPostMed,file="results/mediationPostCorr.RData")
