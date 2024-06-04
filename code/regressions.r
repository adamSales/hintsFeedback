###########################################################
## Main effect models
###########################################################
### for code used in model selection and checking, see:
## posttest.r, stateTst.r, perWorked.r, and perCorr.r

postAll0 <- lm(postS~Z+class,  data=dat, subset=hasPosttest)

postAll <- lm(postS ~ Z + pretestC + Scale_Score5imp + ScaleScore5miss + race +
    MALE + EIP + ESOL + IEP + GIFTED + accelerated + logTime +
    pre_MA_total_scoreimp + pre_MSE_total_scoreimp + pre_PS_tasks_total_scoreimp +
     class,
    data=dat, subset=hasPosttest)

stateAll0 <- lm(Scale_Score7S ~ Z+ class,
    data=dat,subset=hasStatetest)

stateAll <- lm(Scale_Score7S ~ Z + pretestC + Scale_Score5imp + ScaleScore5miss +
    race + MALE + EIP + ESOL + IEP + GIFTED + accelerated + logTime +
    pre_MA_total_scoreimp + pre_MSE_total_scoreimp + pre_PS_tasks_total_scoreimp +
     class,
    data=dat,subset=hasStatetest)

workAll0 <- lm(perWorked ~ Z +class, data=dat)

workAll <- lm(perWorked ~ Z + pretestC + Scale_Score5imp + ScaleScore5miss +
    race + MALE + EIP + ESOL + IEP + GIFTED + accelerated + logTime +
    pre_MA_total_scoreimp + pre_MSE_total_scoreimp + pre_PS_tasks_total_scoreimp +
     class,
    data=dat)

corrAll0 <- lm(perCorr~Z+class,data=dat,subset=hasPerCorr)

corrAll <- lm(perCorr ~ Z + pretestC + Scale_Score5imp + ScaleScore5miss +
    race + MALE + EIP + ESOL + IEP + GIFTED + accelerated + logTime +
    pre_MA_total_scoreimp + pre_MSE_total_scoreimp + pre_PS_tasks_total_scoreimp +
    class,
    data=dat, subset=hasPerCorr)



RQ1mods <- nlist(postAll0,postAll,stateAll0,stateAll)
RQ2mods <- nlist(workAll0,workAll,corrAll0,corrAll)


tabMods <- nlist(Posttest=postAll,Statetest=stateAll,`% Probs. Worked`=workAll,
     `% Probs Correct 1st Try`=corrAll)#%>%#,update(postAll2,subset=hasBothtest),
     ###stateAll2,update(stateAll2,subset=hasBothtest))%>%



###########################################################
## Mediation models
###########################################################


#source('code/mediationWorkPost.r')
#source('code/mediationWorkState.r')
#load('results/workStateMods.RData')
#load('results/workPostModels.RData')


### Mediator: % Worked
dat <- within(dat,{
  perWorkedC <- perWorked-mean(perWorked)
  perCorrC <- perCorr-mean(perCorr,na.rm=TRUE)
  })

## constituent regression models
workPost <- update(workAll,perWorkedC~., subset=hasPosttest)
workState <- update(workAll,perWorkedC~., subset=hasStatetest)

workPostReg <- lm(update(covForm,postS~Z*perWorkedC+.+class-virtual),
                  data=dat,subset=hasPosttest)

workStateReg <- lm(update(covForm,Scale_Score7S~Z*perWorkedC+.+class-virtual),
                   data=dat,subset=hasStatetest)

workMedMods <- nlist(workPost,workState,workPostReg,workStateReg)





### Mediator: % Correct
corrPost <- update(corrAll,perCorrC~.,subset=hasPosttest&hasPerCorr)
corrState <- update(corrAll,perCorrC~.,subset=hasStatetest&hasPerCorr)


corrPostReg <- lm(update(covForm,postS~Z*perCorrC+.+class-virtual),
                  data=dat,subset=hasPosttest&hasPerCorr)

corrStateReg <- lm(update(covForm,Scale_Score7S~Z*perCorrC+.+class-virtual),
                   data=dat,subset=hasStatetest&hasPerCorr)


corrMedMods <- nlist(corrPost,corrState,corrPostReg,corrStateReg)

###########################################################
## save regression models for tables
###########################################################

save(RQ1mods,RQ2mods,tabMods,workMedMods,corrMedMods,file='results/regressions.RData')
