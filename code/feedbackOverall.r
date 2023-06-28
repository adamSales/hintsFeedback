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

source('code/attritionCovariates.r')

source('code/posttest.r')
source('code/stateTest.r')
source('code/perWorked.r')
#source('code/perComp.r')
source('code/perCorr.r')




### regression table
tabMods <- list(Posttest=postAll2,Statetest=stateAll2,`% Probs. Worked`=workAll,
     `% Probs Correct 1st Try`=corrAll2)#%>%#,update(postAll2,subset=hasBothtest),
     ###stateAll2,update(stateAll2,subset=hasBothtest))%>%


inf <- tabMods%>%map(coeftest,vcov.=vcovHC,type='HC')


  #c(postML)%>%
ss <- stargazer(tabMods,se=map(inf,~.[,2]),p=map(inf,~.[,4]),#out="results/regressions.tex",
                ci=FALSE,single.row=FALSE,omit="class",dep.var.labels.include=FALSE,

            column.labels=c("Posttest","State Test","\\% Probs. Worked","\\% Correct 1st Try"),
            digits=3,star.cutoffs=c(.05,0.01,0.001),intercept.bottom=FALSE,
            title="Regressions estimating effect of immediate feedback (Z) on various outcomes. Classroom fixed effects included in model, omitted from table.",omit.stat=c("ser","f"))#%>%do.call("stargazer",.)

ss <- ss[-grep("(1)",ss,fixed=TRUE)]
ss <- ss[-grep("& & & & \\\\",ss,fixed=TRUE)]

cat(ss,file="results/regressions.tex",sep="\n")

#source('code/mediationWorkPost.r')
#source('code/mediationWorkState.r')
load('results/workStateMods.RData')
load('results/workPostModels.RData')
tabMods <- list(workEffectPost,workPostReg,workMod,workStateReg)


inf <- tabMods%>%map(coeftest,vcov.=vcovHC,type='HC')


ss <- stargazer(tabMods,se=map(inf,~.[,2]),p=map(inf,~.[,4]),#out="results/workMedRegressions.tex",
                ci=FALSE,single.row=FALSE,omit=c("class","as.factor(pretestC)"),#dep.var.labels.include=FALSE,
            column.labels=c("Posttest Subset","","State Test Subset"),
            digits=3,star.cutoffs=c(.05,0.01,0.001),intercept.bottom=FALSE,
            title="Regressions for mediation analyses, with \\% Problems Worked as the mediatior and Post and State tests as the outcome. Classroom fixed effects included in model, omitted from table. Fixed effects for individual pretest scores are included in models 2 \\& 4, but omitted from table.",omit.stat=c("ser","f"))#%>%do.call("stargazer",.)
ss <- gsub("covForm &","Posttest &",ss)
ss <- gsub("covForm","State Test",ss)
ss <- gsub("perWorked","\\\\% Probs. Worked",ss)

pretestRows <- grep("as.factor(pretestC)",ss,fixed=TRUE)
pretestRows <- c(pretestRows,pretestRows+1,pretestRows+2)
ss <- ss[-pretestRows]

ss <- ss[-grep("(1)",ss,fixed=TRUE)]
ss <- ss[-grep("& & & & \\\\",ss,fixed=TRUE)]


cat(ss,file='results/workMedRegressions.tex',sep='\n')

load('results/mediationPerWorked.RData')
smat <- print(summary(med))
print(xtable(smat,caption="Mediation analysis results. Mediator: \\% Problems Worked. Outcome: Posttest"),
      file="results/mediationWorkPost.tex")

load('results/mediationPerWorkedState.RData')
smat <- print(summary(med))
print(xtable(smat,caption="Mediation analysis results. Mediator: \\% Problems Worked. Outcome: State Test"),
      file="results/mediationWorkState.tex")


###### Mediator: % Correct

print(load('results/corPostModels.RData'))
corrPost=corrAll2
load('results/corrStateRegressions.RData')
tabMods <- list(corrPost,corrPostReg,corrAll2,corrStateReg)


inf <- tabMods%>%map(coeftest,vcov.=vcovHC,type='HC')


ss <- stargazer(tabMods,se=map(inf,~.[,2]),p=map(inf,~.[,4]),#out="results/workMedRegressions.tex",
                ci=FALSE,single.row=FALSE,omit="class",#dep.var.labels.include=FALSE,
            column.labels=c("Posttest Subset","","State Test Subset",""),
            digits=3,star.cutoffs=c(.05,0.01,0.001),intercept.bottom=FALSE,
            title="Regressions for mediation analyses, with \\% Problems Correct on First Try (i.e. before any feedback) as the mediatior and Post and State tests as the outcome. Classroom fixed effects included in model, omitted from table.",
            omit.stat=c("ser","f"))#%>%do.call("stargazer",.)
ss[12] <- "\\\\[-1.8ex] & \\% Correct & Posttest & \\% Correct & State Test \\\\ "
ss <- gsub("covForm &","Posttest &",ss)
ss <- gsub("covForm","State Test",ss)
ss <- gsub("perCorr","\\\\% Correct 1st Try",ss)

ss <- ss[-grep("(1)",ss,fixed=TRUE)]
ss <- ss[-grep("& & & & \\\\",ss,fixed=TRUE)]


cat(ss,file='results/corrMedRegressions.tex',sep='\n')

print(load('results/mediationPostCorr.RData'))
smat <- print(summary(med))
print(xtable(smat,caption="Mediation analysis results. Mediator: \\% Problems Correct on 1st Try. Outcome: Posttest"),
      file="results/mediationCorrPost.tex")

load('results/mediationStateCorr.RData')
smat <- print(summary(med))
print(xtable(smat,caption="Mediation analysis results. Mediator: \\% Problems Correct on 1st Try. Outcome: State Test"),
      file="results/mediationCorrState.tex")







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
