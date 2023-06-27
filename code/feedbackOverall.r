library(tidyverse)
library(RItools)
library(mosaic)
library(lme4)
library(texreg)
library(table1)
library(xtable)
library(lmtest)
library(estimatr)
library(stargazer)
library(sandwich)
library(loop.estimator)

source("code/functions.r")


load('data/feedbackData.RData')

covNames <-
  c("pretestC","Scale_Score5imp","ScaleScore5miss","race","MALE","EIP","ESOL","IEP","GIFTED","accelerated","virtual","logTime","pre_MA_total_scoreimp","pre_MSE_total_scoreimp","pre_PS_tasks_total_scoreimp","pretestMiss") ## more?

covForm <- as.formula(paste("I(Z==1)~",paste(covNames,collapse="+")))

source('code/posttest.r')
source('code/stateTest.r')
source('code/perWorked.r')
source('code/perComp.r')
source('code/perCorr.r')




### regression table
list(postAll2)%>% #,update(postAll2,subset=hasBothtest),
     ###stateAll2,update(stateAll2,subset=hasBothtest))%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  #c(postML)%>%
  stargazer(out="results/regressions.tex",ci=FALSE,single.row=FALSE,omit="class",
            type='latex',digits=3,star.cutoffs=c(.1,.05,0.01,0.001),intercept.bottom=FALSE)



### estimates
list(post0,post2,post0b,post2b,state0,state2,state0b,state2b)%>%
  map(coeftest,vcov.=vcovHC,type='HC')%>%
  map(function(x) round(x['Z',],3))


### CIs
list(post0,post2,post0b,post2b,state0,state2,state0b,state2b)%>%
  map(coefci,"ZImmediate",vcov.=vcovHC,type='HC')%>%
  map(round, digits=3)


## #### mlm


## datLong=dat%>%
##   pivot_longer(
##     c(mid,post,ScaleScore7),names_to="test",values_to="score")

## gmod1=lmer(score~Z*test+Scale_Score5imp+pretestC+FEMALE+race+EIP+ESOL+IEP+GIFTED+AbsentDays5+MOBILE5+
##              class+
##              (1|student_number),
##               data=datLong)
## VarCorr(gmod1)
## summary(gmod1)$coef[!startsWith(names(fixef(gmod1)),'class'),]
## ### estimates by time point
## ### lazy: just rerun model after changing ref
## gmod1a=update(gmod1,data=mutate(datLong,test=fct_relevel(test,'post')))
## summary(gmod1a)$coef['ZImmediate',]

## gmod1b=update(gmod1,data=mutate(datLong,test=fct_relevel(test,'ScaleScore7')))
## summary(gmod1b)$coef['ZImmediate',]

## gmod2=update(gmod1,.~.-(1|student_number)+(test|student_number))
