
###########################################################
####### Actual mediation analyses
###########################################################
if(!exists("workMedMods")|!exists("corrMedMods")) load('results/regressions.RData')
attach(workMedMods)
attach(corrMedMods)

vcov <- function(object,...) vcovHC(object,type='HC0',...)
medWorkPost <- mediate(model.m=workPost,model.y=workPostReg,treat="Z",mediator="perWorkedC",data=dat,subset=hasPosttest)
medWorkState <- mediate(model.m=workState,model.y=workStateReg,treat="Z",mediator="perWorkedC",data=dat,subset=hasStatetest)
medCorrPost <- mediate(model.m=corrPost,model.y=corrPostReg,treat="Z",mediator="perCorrC",data=dat,subset=hasPosttest&hasPerCorr)
medCorrState <- mediate(model.m=corrState,model.y=corrStateReg,treat="Z",mediator="perCorrC",data=dat,subset=hasStatetest&hasPerCorr)

detach(workMedMods)
detach(corrMedMods)


save(medWorkPost,medWorkState,medCorrPost,medCorrState,file='results/mediations.RData')

## medSensWorkPost <- medsens(medWorkPost)
## medSensWorkState <- medsens(medWorkState)
## medSensCorrPost <- medsens(medCorrPost)
## medSensCorrState <- medsens(medCorrState)

## save(medSensWorkPost,medSensWorkState,medSensCorrPost,medSensCorrState,file='results/mediationSensitivity.RData')
