library(tidyverse)
library(RItools)
library(mosaic)
library(lme4)
library(lmerTest)
library(estimatr)
library(texreg)


source("code/functions.r")

Scale <- function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)

psd=function(Z,Y)
  sqrt((var(Y[Z=='BAU'],na.rm=TRUE)*(sum(Z[!is.na(Y)]=='BAU')-1)+var(Y[Z=='ASSISTments'],na.rm=TRUE)*(sum(Z[!is.na(Y)]=='ASSISTments')-1))/(sum(!is.na(Y))-2)))


load('data/feedbackData.RData')

dat=mutate(dat,
           race=raceEthnicityFed%>%
             factor()%>%
             fct_recode(
               `Hispanic/Latino`="1",
                `American Indian/Alaska Native`="2",
                `Asian`="3",
                `Black/African American`="4",
                `Native Hawaiian or Other Pacific islander`="5",
                `White`="6",
               `Two or more races`="7")%>%
             fct_explicit_na("Unknown")%>%
             fct_relevel('White'),
           raceLump=fct_lump(race,2),
           pretest=pre.total_math_score,
           midtest=mid.total_math_score,
           posttest=post.total_math_score,
           pretestC = pretest-round(mean(pretest,na.rm=TRUE)),#Scale(pretest),
           pretestPos = pretest-round(mean(pretest,na.rm=TRUE)+sd(pretest,na.rm=TRUE)),
           pretestNeg = pretest-round(mean(pretest,na.rm=TRUE)-sd(pretest,na.rm=TRUE)),
           Female=ifelse(Gender=='F',1,0),
           ScaleScore5=ifelse(is.na(Scale.Score5),0,scale(Scale.Score5)),
           ss5na=ifelse(is.na(Scale.Score5),1,0),
           gradeLevel=ifelse(Performance.Level5%in%c('Proficient Learner','Distinguished Learner'),1,0)
           )%>%
  droplevels()



### midtest models
mod0=lm_robust(midtest~Z,data=dat,fixed_effects=~class)
mod1=update(mod0,.~.+pretestC+ESOL+ScaleScore5+ss5na+EIP+IEP+Female+gradeLevel)
mod2=update(mod1,.~.-pretestC-Z+Z*pretestC)

mid=list(mod0=mod0,mod1=mod1,mod2=mod2)

post=map(mid,function(x) update(x,posttest~.,data=dat))

state=map(mid,function(x) update(x,ScaleScore7~.,data=dat))

datLong=dat%>%
  mutate(
    midtestS=scale(midtest,scale=psd(Z,midtest)),
    posttestS=scale(posttest,scale=psd(Z,posttest))
    )%>%
  pivot_longer(
    c(midtestS,posttestS,ScaleScore7),names_to="test",values_to="score")

gmod1=lmer(score~Z*test+ScaleScore5+pretest+FEMALE+race+EIP+ESOL+IEP+GIFTED+ss5na+
             as.factor(class)+
             (1|StuID),
              data=datLong)

VarCorr(gmod1)
summary(gmod1)$coef[!startsWith(names(fixef(gmod1)),'class'),]
### estimates by time point
### lazy: just rerun model after changing ref
gmod1a=update(gmod1,data=mutate(datLong,test=fct_relevel(factor(test),'posttestS')))
summary(gmod1a)$coef['ZASSISTments',]

gmod1b=update(gmod1,data=mutate(datLong,test=fct_relevel(test,'ScaleScore7')))
summary(gmod1b)$coef['ZASSISTments',]

gmod2=dat%>%
  mutate(
    pretestS=scale(pretest,scale=psd(Z,pretest)),
    midtestS=scale(midtest,scale=psd(Z,midtest)),
    posttestS=scale(posttest,scale=psd(Z,posttest))
    )%>%
  pivot_longer(
    c(pretestS,midtestS,posttestS,ScaleScore7),names_to="test",values_to="score")%>%
  mutate(test=fct_relevel(factor(test),'pretestS'))%>%
  lmer(score~Z*test+ScaleScore5+FEMALE+race+EIP+ESOL+IEP+GIFTED+ss5na+
             as.factor(class)+
             (1|StuID),
              data=.)



### plot effects for 3 tests
effs=map_dfr(list(gmod1,gmod1a,gmod1b),~c(fixef(.)['ZASSISTments'],confint(.,parm='ZASSISTments')))

effs=cbind(
  test=c('mid','post','state'),
  effs)

names(effs)=c('test','est','ciL','ciH')

effs$gr=1

ggplot(effs,aes(test,est,ymin=ciL,ymax=ciH,group=gr))+geom_point()+geom_line()+geom_errorbar(width=0)+
  geom_hline(yintercept=0)+ylab("Effect Size (d)")


effs=data.frame(
                eff=


### covariates in poster
datP=filter(dat,hasMidtest)%>%droplevels()
tab=function(...) table(...,useNA='ifany')
perc=function(vvv) round(tab(vvv)/length(vvv)*100,1)
perc(datP$Z)
perc(datP$Gender)
sort(perc(datP$race),dec=TRUE)

### models in poster

t.test(datP$midtest-datP$pretest)


list(mod0,mod1,mod2)%>%map(~round(c(.$coef['ZASSISTments'],.$conf.high['ZASSISTments']-.$coef['ZASSISTments']),3))

htmlreg(list(mod0,mod1,mod2),file='posterModels.html',digits=3,stars = c(0.001,0.01,0.05,0.1),symbol='+',
        single.row=TRUE,include.ci=FALSE,reorder.coef=c(1,10,2,3,4,6,7,8,9,5))

### cohen's d
confint(mod1,'ZASSISTments')/psd(datP$Z,datP$midtest)

round(c(mod2$coef['ZASSISTments:pretestC'],mod2$conf.high['ZASSISTments:pretestC']-mod2$coef['ZASSISTments:pretestC']),3)
mod2








### sample sizes
ns=data.frame(
  test=c('mid','post','state','total'),
  n=c(
    sapply(select(dat,hasMidtest,hasPosttest,hasStatetest),sum),
    with(dat, sum(hasMidtest|hasPosttest|hasStatetest)))
  )


### attrition analysis
att=rbind(
  cbind(
    mid=1-mean(~hasMidtest|Z,data=dat),
    post=1-mean(~hasPosttest|Z,data=dat),
    state=1-mean(~hasStatetest|Z,data=dat)),
  all=1-sapply(select(dat,hasMidtest,hasPosttest,hasStatetest),mean))


### plot % observed
dat%>%
  bind_rows(mutate(dat,Z='All'))%>%
  group_by(Z)%>%
  summarize(across(starts_with('has'),mean))%>%
  select(-hasPretest)%>%
  pivot_longer(-Z,names_to='Outcome',names_prefix='has',values_to="perObs")%>%
  ungroup()%>%
  ggplot(aes(Outcome,perObs,color=Z,group=Z))+geom_point()+geom_line()

### plot overall & differential attrition vs WWC standards
plotWWC(ov=att['all',],diff=apply(att,2,function(x) x['ASSISTments']-x['BAU']),
        labs=colnames(att))


#### covariate balance
covNames <-
    c("pretest","ScaleScore5","race","FEMALE","EIP","ESOL","IEP","GIFTED") ## more?

#### process some covariates
dat=mutate(dat,
           race=raceEthnicityFed%>%
               factor()%>%
               fct_lump_min(100)%>%
               fct_recode(`Hispanic/Latino`="1",Asian="3",White="6")%>%
               fct_relevel('White'),
           pretest = pre.total_math_score-round(mean(pre.total_math_score,na.rm=TRUE)),#Scale(pre.total_math_score),
           pretestPos = pre.total_math_score-round(mean(pre.total_math_score,na.rm=TRUE)+sd(pre.total_math_score,na.rm=TRUE)),
           pretestNeg = pre.total_math_score-round(mean(pre.total_math_score,na.rm=TRUE)-sd(pre.total_math_score,na.rm=TRUE)),
           `Race/Ethnicity`=factor(raceEthnicityFed)%>%
               fct_recode(
                   `Hispanic/Latino`="1",
                   `American Indian/Alaska Native`="2",
                   Asian="3",
                   `Black/African American`="4",
                   `Native Hawaiian or Other Pacific islander`="5",
                   White="6",
                   `Two or more races`="7")%>%
               fct_explicit_na("Unknown"),
           Gender=factor(ifelse(FEMALE==1,'Female','Male'))%>%fct_explicit_na("Unknown"),
           Z=fct_relevel(Z,'BAU'))

dat <- mutate(dat,
               teach=ifelse(is.na(initial_teacher_id),final_teacher_id,initial_teacher_id),
               class=ifelse(is.na(initial_teacher_class),final_teacher_class,initial_teacher_class))


dat$ScaleScore5=dat[['Scale.Score5']]

dat=dat%>%group_by(class)%>%mutate(mm=mean(ScaleScore5,na.rm=TRUE),
                                   ScaleScore5imp=ifelse(is.na(ScaleScore5),
                                                         mm,ScaleScore5))%>%
  select(-mm)%>%
  ungroup()


covForm <- as.formula(paste("I(Z=='ASSISTments')~",paste(covNames,collapse="+")))


### midtest
bals <-
  map(c('Mid','Post','State')%>%setNames(.,.),
      function(test)
        xBalance(covForm,
                 data=dat[dat[[paste0('has',test,'test')]],],
                 report=c('std.diffs','z.scores','chisquare.test'),
                 strata=list(cls=~class)))

map_dfr(names(bals),
        function(nn)
          as.data.frame(RItools:::prepareXbalForPlot(bals[[nn]]))%>%
          rownames_to_column()%>%
          mutate(test=nn))%>%
  ggplot(aes(y = rowname, x = cls)) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept=c(-.25,-.05,.25,.05),linetype='dotted')+
  geom_point() +
  theme(legend.position = "bottom")+
  facet_wrap(~test,nrow=1)+theme_bw()+xlab("Standardized Difference")+ylab(NULL)

### which variables are imbalanced with |stand. diff|>0.05?
bals%>%
  map(~rownames(.$results)[abs(.$results[,'std.diff',1])>0.05])%>%
  unlist()%>%unique()

#### estimate effects
dat$post=Scale(dat$post.total_math_score)
dat$mid=Scale(dat$mid.total_math_score)

### midtest
mid0=lm_robust(mid~Z,data=dat,subset=hasMidtest,fixed_effects=~class)
mid1=lm_robust(mid~Z+ScaleScore5imp+pretest,data=dat,subset=hasMidtest,fixed_effects=~class)
mid2=lm_robust(mid~Z+ScaleScore5imp+pretest+FEMALE+race+ESOL+IEP+GIFTED+AbsentDays5+MOBILE5,data=dat,subset=hasMidtest,fixed_effects=~class)
screenreg(list(mid0,mid1,mid2))
mid1=lm_robust(mid~Z*pretest+ScaleScore5imp,data=dat,subset=hasMidtest,fixed_effects=~class)


### posttest
post0=lm_robust(post~Z,data=dat,subset=hasPosttest,fixed_effects=~class)
post1=lm_robust(post~Z+ScaleScore5imp+pretest,data=dat,subset=hasPosttest,fixed_effects=~class)
post2=lm_robust(post~Z+ScaleScore5imp+pretest+FEMALE+race+EIP+ESOL+IEP+GIFTED+AbsentDays5+MOBILE5,data=dat,subset=hasPosttest,fixed_effects=~class)
screenreg(list(post0,post1,post2))
postInt=lm_robust(post~Z*pretest+ScaleScore5imp,data=dat,subset=hasPosttest,fixed_effects=~class)
screenreg(list(post0,post1,post2,postInt))


### state test
state0=lm_robust(ScaleScore7~Z,data=dat,subset=hasStatetest,fixed_effects=~class)
state1=lm_robust(ScaleScore7~Z+ScaleScore5imp+pretest,data=dat,subset=hasStatetest,fixed_effects=~class)
state2=lm_robust(ScaleScore7~Z+ScaleScore5imp+pretest+FEMALE+race+EIP+ESOL+IEP+GIFTED+AbsentDays5+MOBILE5,data=dat,subset=hasStatetest,fixed_effects=~class)
screenreg(list(state0,state1,state2))
stateImp=lm_robust(ScaleScore7~Z*pretest+ScaleScore5imp,data=dat,subset=hasStatetest,fixed_effects=~class)
screenreg(list(state0,state1,state2,stateImp))


#### mlm
