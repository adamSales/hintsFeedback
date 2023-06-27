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
mmAll <- model.matrix(corrAll)
diagnosticAll <-
  data.frame(x=fitted(corrAll),y=resid(corrAll),nn='main')

for(i in 2:ncol(mmAll))
  if(n_distinct(mmAll[,i])>2){
    mod1=lm(datCorr$perCorr~mmAll[,-i])
    mod2=lm(mmAll[,i]~mmAll[,-i])
    diagnosticAll <-
      bind_rows(diagnosticAll,
                data.frame(x=resid(mod2),y=resid(mod1),
                           nn=colnames(mmAll)[i]))
    }

diagnosticAll%>%
  filter(nn!="main")%>%
  ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
  geom_hline(yintercept=0)+
  facet_wrap(~nn,scales="free")

diagnosticAll%>%
  filter(nn=="main")%>%
  ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
  geom_hline(yintercept=0)+
  labs(x="residuals",y="fitted values")

datCorr%>%
  mutate(Z=as.factor(Z))%>%
  group_by(pretestC,Z)%>%
  summarize(Y=mean(perCorr,na.rm=TRUE),n=n())%>%
  ggplot(aes(pretestC,Y,color=Z))+
  geom_point(aes(size=n))+
  geom_smooth()

datCorr%>%
  mutate(Z=as.factor(Z))%>%
  group_by(Scale_Score5imp,Z)%>%
  summarize(Y=mean(perCorr,na.rm=TRUE),n=n())%>%
  ggplot(aes(Scale_Score5imp,Y,color=Z))+
  geom_point(aes(size=n))+
  geom_smooth()


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
