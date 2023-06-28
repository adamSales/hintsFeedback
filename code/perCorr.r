################### missigness/attrition

xtabs(~nworked+ncomp,subset(dat,!hasPerCorr))

xtabs(~hasPosttest+hasPerCorr,dat)

xtabs(~hasStatetest+hasPerCorr,dat)

xtabs(~hasBothtest+hasPerCorr,dat)

datCorr <- dat%>%filter(hasPerCorr)%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))



corr0=lm(perCorr~Z+class,datCorr)

corr1=lm(perCorr~Z+Scale_Score5imp+pretestC+class,datCorr)

corrAll=lm(update(covForm,perCorr~Z+.+class-virtual),data=datCorr)

### diagnostic plots
#diagPlots(corrAll)

#posPart=function(x) ifelse(x>0,x,0)


mm <- model.matrix(corrAll)
mm <- mm[,-c(1,2,grep("class",colnames(mm)))]
mm <- scale(mm)
corrLin <- lm(perCorr~Z*mm+class,data=datCorr)


LOOP <- loop(datCorr$perCorr,
             datCorr$Z,
             model.matrix(corrAll)[,-c(1,2)])
c(LOOP[1],sqrt(LOOP[2]))

corrML <- lmer(update(formula(corrAll),.~.-class+(1|class)),
               datCorr)

list(corr0,corrAll,corrLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(corrML)%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)

list(corr0,corrAll,corrLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(corr0,corrAll,corrLin)%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)



list(corr0,corrAll,corrLin)%>%
  map(~update(.,subset=hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(update(corrML,subset=hasBothtest))%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)


list(corr0,corrAll,corrLin)%>%
  map(~update(.,subset=datCorr$hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(corr0,corrAll,corrLin)%>%
    map(~update(.,subset=datCorr$hasBothtest))%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)

save(corr0,corrAll,corrLin,corrML,LOOP,file="corrModels.RData")
