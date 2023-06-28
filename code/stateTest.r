### statetest
##
datState <- dat%>%filter(hasStatetest)%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))

state0=lm(Scale_Score7S~Z+class,datState)

state1=lm(Scale_Score7S~Z+Scale_Score5imp+pretestC+class,datState)
state2=lm(as.formula(paste0("Scale_Score7S~Z+",paste(adj$State,collapse="+"),"+class")),datState)

stateAll=lm(update(covForm,Scale_Score7S~Z+.+class-virtual),data=datState)

### diagnostic plots
#diagPlots(stateAll)

posPart=function(x) ifelse(x>0,x,0)

stateAll2=update(stateAll,.~.+posPart(pretestC+1))

data.frame(x=fitted(stateAll2),y=resid(stateAll2))%>%
   ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
  geom_hline(yintercept=0)+
  labs(x="residuals",y="fitted values")

mm <- model.matrix(stateAll2)
mm <- mm[,-c(1,2,grep("class",colnames(mm)))]
mm <- scale(mm)
stateLin <- lm(Scale_Score7S~Z*mm+class,data=datState)


LOOP <- loop(datState$Scale_Score7S,
             datState$Z,
             model.matrix(stateAll2)[,-c(1,2)])
c(LOOP[1],sqrt(LOOP[2]))

stateML <- lmer(update(formula(stateAll2),.~.-class+(1|class)),
               datState)

list(state0,state2,stateAll2,stateLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(stateML)%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)

list(state0,state2,stateAll2,stateLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(state0,state2,stateAll2,stateLin)%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)



list(state0,state2,stateAll2,stateLin)%>%
  map(~update(.,subset=hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(update(stateML,subset=hasBothtest))%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)


list(state0,state2,stateAll2,stateLin)%>%
  map(~update(.,subset=datState$hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(state0,state2,stateAll2,stateLin)%>%
    map(~update(.,subset=datState$hasBothtest))%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)

save(state0,state2,stateAll2,stateLin,stateML,LOOP,file='results/stateTestMods.RData')
