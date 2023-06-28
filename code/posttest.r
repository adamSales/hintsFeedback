### posttest
##
datPost <- dat%>%filter(hasPosttest)%>%group_by(class)%>%
  mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))

post0=lm(postS~Z+class,datPost)

post1=lm(postS~Z+Scale_Score5imp+pretestC+class,datPost)
post2=lm(as.formula(paste0("postS~Z+",paste(adj$Post,collapse="+"),"+class")),datPost)

postAll=lm(update(covForm,postS~Z+.+class-virtual),data=datPost)

### diagnostic plots
#diagPlots(postAll)


posPart=function(x) ifelse(x>0,x,0)

postAll2=update(postAll,.~.+posPart(pretestC+1))

data.frame(x=fitted(postAll2),y=resid(postAll2))%>%
   ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
  geom_hline(yintercept=0)+
  labs(x="residuals",y="fitted values")

mm <- model.matrix(postAll2)
mm <- mm[,-c(1,2,grep("class",colnames(mm)))]
mm <- scale(mm)
postLin <- lm(postS~Z*mm+class,data=datPost)


LOOP <- loop(datPost$postS,
             datPost$Z,
             model.matrix(postAll2)[,-c(1,2)])
c(LOOP[1],sqrt(LOOP[2]))

postML <- lmer(update(formula(postAll2),.~.-class+(1|class)),
               datPost)

list(post0,post2,postAll2,postLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(postML)%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)

## estimates
list(post0,post2,postAll2,postLin)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(post0,post2,postAll2,postLin)%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)


list(post0,post2,postAll2,postLin)%>%
  map(~update(.,subset=datPost$hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  c(update(postML,subset=hasBothtest))%>%
  stargazer(ci=FALSE,single.row=FALSE,omit="class",
            type='text',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)


list(post0,post2,postAll2,postLin)%>%
  map(~update(.,subset=datPost$hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
   map_dfr(function(x) round(x['Z',],3))

list(post0,post2,postAll2,postLin)%>%
    map(~update(.,subset=datPost$hasBothtest))%>%
  map(coefci,"Z",vcov.=vcovHC,type='HC')%>%
   map(round,digits=3)%>%do.call("rbind",.)

save(post0,post2,postAll2,postLin,LOOP,postML,file='results/postTestMods.RData')
