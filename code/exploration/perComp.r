### comptest
##
datP <- dat%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))

comp0=lm(perComp~Z+class,datP)

comp1=lm(perComp~Z+Scale_Score5imp+pretestC+class,datP)

compAll=lm(update(covForm,perComp~Z+.+class-virtual),data=datP)

### diagnostic plots
#diagPlots(compAll)


compAll2=update(compAll,.~.-pretestC+as.factor(pretestC))

data.frame(x=fitted(compAll2),y=resid(compAll2))%>%
   ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
  geom_hline(yintercept=0)+
  labs(x="residuals",y="fitted values")

mm <- model.matrix(compAll2)
mm <- mm[,-c(1,2,grep("class",colnames(mm)))]
mm <- scale(mm)
compLin <- lm(perComp~Z*mm+class,data=datP)


LOOP <- loop(datP$perComp,
             datP$Z,
             model.matrix(compAll2)[,-c(1,2)])
c(LOOP[1],sqrt(LOOP[2]))

compML <- lmer(update(formula(compAll2),.~.-class+(1|class)),
               datP)

list(comp0,compAll2,compLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(compML)%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)

list(comp0,compAll2,compLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(comp0,compAll2,compLin)%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)



list(comp0,compAll2,compLin)%>%
  map(~update(.,subset=hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(update(compML,subset=hasBothtest))%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)


list(comp0,compAll2,compLin)%>%
  map(~update(.,subset=datP$hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(comp0,compAll2,compLin)%>%
    map(~update(.,subset=datP$hasBothtest))%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)
