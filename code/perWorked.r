### worktest
##
datP <- dat%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))

work0=lm(perWorked~Z+class,datP)

work1=lm(perWorked~Z+Scale_Score5imp+pretestC+class,datP)

workAll=lm(update(covForm,perWorked~Z+.+class-virtual),data=datP)

### diagnostic plots
diagPlots(workAll)

datP%>%
  mutate(Z=as.factor(Z))%>%
  group_by(pretestC,Z)%>%
  summarize(Y=mean(perWorked,na.rm=TRUE),n=n())%>%
  ggplot(aes(pretestC,Y,color=Z))+
  geom_point(aes(size=n))+
  geom_smooth()

#posPart=function(x) ifelse(x>0,x,0)

workAll2=update(workAll,.~.-pretestC+as.factor(pretestC))

data.frame(x=fitted(workAll2),y=resid(workAll2))%>%
   ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
  geom_hline(yintercept=0)+
  labs(x="residuals",y="fitted values")

mm <- model.matrix(workAll2)
mm <- mm[,-c(1,2,grep("class",colnames(mm)))]
mm <- scale(mm)
workLin <- lm(perWorked~Z*mm+class,data=datP)


LOOP <- loop(datP$perWorked,
             datP$Z,
             model.matrix(workAll2)[,-c(1,2)])
c(LOOP[1],sqrt(LOOP[2]))

workML <- lmer(update(formula(workAll2),.~.-class+(1|class)),
               datP)

list(work0,workAll2,workLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(workML)%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)

list(work0,workAll2,workLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(work0,workAll2,workLin)%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)



list(work0,workAll2,workLin)%>%
  map(~update(.,subset=hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(update(workML,subset=hasBothtest))%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)


list(work0,workAll2,workLin)%>%
  map(~update(.,subset=datP$hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(work0,workAll2,work@Lin)%>%
    map(~update(.,subset=datP$hasBothtest))%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)
