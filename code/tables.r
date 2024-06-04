load('results/regressions.RData')
load('results/mediations.RData')
load('results/sensitivity.RData')
load('results/raschEff2.RData')

#########################
## Main regression table for appendix
#######################

regTab<- regTabFunc(tabMods,Title="Regressions estimating effect of immediate feedback on various outcomes. Classroom fixed effects included in model, omitted from table.")

cat(regTab,file="tables/regressions.tex",sep="\n")
system("pandoc tables/regressions.tex -o tables/regressions.html")

######################
## main effects table
######################

regTabSmall <- c(
"\\documentstyle{article}",
"\\begin{document}",
  regTab[1:12],
                 gsub('Immediate Feedback','Effect',regTab[16]),
                 paste0('SE',gsub('\\(|\\)','',regTab[17])),
                 paste(paste(
                   c('CI',vapply(attributes(regTab)$cis,function(x) paste0('(',fround(x['Z',1],2),',',fround(x['Z',2],2),')'),'a')),
                   collapse=' & '),"\\\\"),
                 '\\hline',
                 regTab[grep('Observations',regTab)],
tail(regTab,5),
"\\end{document}")
cat(regTabSmall,file="tables/regSmall.tex",sep="\n")
system("pandoc tables/regSmall.tex -o tables/regSmall.html")



######################
## RQ1 effects table
######################
rq1tab<- regTabFunc(RQ1mods)
rq1tabSmall <- c(
"\\documentstyle{article}",
"\\begin{document}",
rq1tab[1:11],
paste("Outcome",rq1tab[12]),
"Covariate Controls & No &Yes &No&Yes \\\\",
"\\hline\\hline",
                 gsub('Immediate Feedback','Effect',rq1tab[16]),
                 paste0('SE',gsub('\\(|\\)','',rq1tab[17])),
                 paste(paste(
                   c('CI',vapply(attributes(rq1tab)$cis,function(x) paste0('(',fround(x['Z',1],2),',',fround(x['Z',2],2),')'),'a')),
                   collapse=' & '),"\\\\"),
                 '\\hline',
                 rq1tab[grep('Observations',rq1tab)],
tail(rq1tab,5),
"\\end{document}")

cat(rq1tabSmall,file="tables/rq1Small.tex",sep="\n")
system("pandoc tables/rq1Small.tex -o tables/rq1Small.html")

######
### results for body of the text
######
atePost0 <- getEff(rq1tab,'postAll0')
atePost <- getEff(rq1tab,'postAll')
ateState0 <- getEff(rq1tab,'stateAll0')
ateState <- getEff(rq1tab,'stateAll')

######################
## RQ2 effects table
######################
rq2tab <- regTabFunc(RQ2mods)
rq2tabSmall <- c(
"\\documentstyle{article}",
"\\begin{document}",
rq2tab[1:11],
paste("Outcome",rq2tab[12]),
"Covariate Controls & No &Yes &No&Yes \\\\",
"\\hline\\hline",
                 gsub('Immediate Feedback','Effect',rq2tab[16]),
                 paste0('SE',gsub('\\(|\\)','',rq2tab[17])),
                 paste(paste(
                   c('CI',vapply(attributes(rq2tab)$cis,function(x) paste0('(',fround(x['Z',1],2),',',fround(x['Z',2],2),')'),'a')),
                   collapse=' & '),"\\\\"),
                 '\\hline',
                 rq2tab[grep('Observations',rq2tab)],
tail(rq2tab,5),
"\\end{document}")

cat(rq2tabSmall,file="tables/rq2Small.tex",sep="\n")
system("pandoc tables/rq2Small.tex -o tables/rq2Small.html")

######
### results for body of the text
######
ateWork0 <- getEff(rq2tab,'workAll0')
ateWork <- getEff(rq2tab,'workAll')
ateCorr0 <- getEff(rq2tab,'corrAll0')
ateCorr <- getEff(rq2tab,'corrAll')

irt <- paste0('(odds ratio=',fround(exp(raschCoefTest$beta)),'; ',
              '$t_{',round(raschCoefTest$df_Satt),'}$=',fround(raschCoefTest$tstat),'; ',
              'p=',fround(raschCoefTest$p_Satt),')')
irtProb <- fround(probScale*100,2)

######################
## Regression table for % problems worked as a mediator
######################

workMedTab <- regTabFunc(workMedMods,
                         Title="Regression models for mediation analyses with \\% of practice problems worked as the mediator. Classroom fixed effects were included in the models but omitted from the table.")

workMedTab <- gsub('\\multicolumn{2}{c}{covForm}','Posttest & State Test',workMedTab,fixed=TRUE)

workMedTab <- gsub('Observations','Subset & Took Posttest & Took State Test&  Took Posttest & Took State Test\\\\
Observations',workMedTab,fixed=TRUE)

workMedTab <- gsub("\\\\[-1.8ex] & \\multicolumn{2}{c}{\\% Probs. Worked}",
     "Dependent Variable & \\% Probs. Worked& \\% Probs. Worked",
     workMedTab,fixed=TRUE)

cat(workMedTab,file='tables/workMedTab.tex',sep='\n')

system("pandoc tables/workMedTab.tex -o tables/workMedTab.html")

######
### results for body of the text
######
sdPerWorked <- fround(sd(dat$perWorkedC,na.rm=TRUE),1)
effWorkedPost <- getEff(workMedTab,'workPostReg','perWorkedC',mult=sd(dat$perWorkedC,na.rm=TRUE))
effWorkedState <- getEff(workMedTab,'workStateReg','perWorkedC',mult=sd(dat$perWorkedC,na.rm=TRUE))

######################
## Regression table for % problems correct as a mediator
######################

corrMedTab <- regTabFunc(corrMedMods,
                         Title="Regression models for mediation analyses with \\% of practice problems answered correctly on the first attempt as the mediator. Classroom fixed effects were included in the models but omitted from the table.")

corrMedTab <- gsub('\\multicolumn{2}{c}{covForm}','Posttest & State Test',corrMedTab,fixed=TRUE)

corrMedTab <- gsub('Observations','Subset & Posttest & State Test&  Posttest & State Test\\\\
& \\& \\%Correct &  \\& \\%Correct&   \\& \\%Correct &  \\& \\%Correct\\\\
Observations',corrMedTab,fixed=TRUE)

corrMedTab <- gsub("\\\\[-1.8ex] & \\multicolumn{2}{c}{\\% Correct 1st Try}",
     "Dependent Variable & \\% Correct & \\% Correct",
     corrMedTab,fixed=TRUE)

cat(corrMedTab,file='tables/corrMedTab.tex',sep='\n')

system("pandoc tables/corrMedTab.tex -o tables/corrMedTab.html")

######
### results for body of the text
######
sdPerCorr <- fround(sd(dat$perCorrC,na.rm=TRUE),1)
effCorrPost <- getEff(corrMedTab,'corrPostReg','perCorrC',mult=sd(dat$perCorrC,na.rm=TRUE))
effCorrState <- getEff(corrMedTab,'corrStateReg','perCorrC',mult=sd(dat$perCorrC,na.rm=TRUE))



######################
## Mediation Analysis Results
######################

### mediation figures
knitr::knit('medFigs.rmd')
rmarkdown::render('medFigs.md')

### medation table
meds <- list(medWorkPost,medCorrPost,medWorkState,medCorrState)
medTab <- data.frame(
  outcome = rep(c('Posttest','State Test'),each=2),
  Mediator = rep(c('% Worked','% Correct'),2),
  `Indirect Effect`= map_chr(meds,~paste0(sprintf("%.3f",.$d.avg),stars(.$d.avg.p))),
  indirect_ci=map_chr(meds,~prtci(.$d.avg.ci)),
   `Direct Effect`= map_chr(meds,~paste0(sprintf("%.3f",.$z.avg),stars(.$z.avg.p))),
  direct_ci=map_chr(meds,~prtci(.$z.avg.ci)),
  n=map_int(meds,~nrow(.$model.y$model)))

#####
## for body of text
####
medText <- function(med,ind){
  stats <- med[paste0(if(ind) 'd.avg' else 'z.avg',c('','.p','.ci'))]
  paste0(
    fround(abs(stats[[1]])),' pooled standard deviations ',
    '(p',ifelse(stats[[2]]<0.001,'<0.001',fround(stats[[2]])),',',
    '95% CI: ',prtci(stats[[3]],3,FALSE),')')
}

workPostInd <- medText(medWorkPost,TRUE)
workStateInd <- medText(medWorkState,TRUE)
corrPostInd <- medText(medCorrPost,TRUE)
corrStateInd <- medText(medCorrState,TRUE)

workPostDir <- medText(medWorkPost,FALSE)
workStateDir <- medText(medWorkState,FALSE)
corrPostDir <- medText(medCorrPost,FALSE)
corrStateDir <- medText(medCorrState,FALSE)



t(medTab[,-1])%>%
  `rownames<-`(c('Mediator: ',
                 'Indirect Effect','','Direct Effect',' ',
                'n'))%>%
  kbl()%>%#row.names=FALSE)%>%
    kable_styling(full_width=FALSE)%>%
  add_header_above(c(" "=1,
                     "Posttest"=2,"State Test"=2))%>%
  add_header_above(c(" ","Outcome:"=2))%>%
  #pack_rows("Mediator",1,1)%>%
  #pack_rows("Indirect Effect",2,3)%>%pack_rows("Direct Effect",4,5)%>%
  column_spec(3,border_right=TRUE)%>%
  column_spec(1,5,bold=TRUE)%>%#c(TRUE,rep(FALSE,4),TRUE))%>%
  row_spec(5,extra_css = "border-bottom: 1px solid")%>%
  save_kable(file = "tables/mediation.html")


######################
## Sensitivity Analysis Results
######################

sink('tables/sensitivity.html')
sensitivity%>%
  map_dfr(~mutate(.x$sensitivity_stats,Outcome=.x$info$formula[[2]]%>%as.character()))%>%
  select(Mediator=treatment,Outcome,Estimate=estimate,SE=se,r2yd.x,rv_q,rv_qa)%>%
  mutate(
    Estimate=Estimate*100,
    SE=SE*100,
    Mediator=ifelse(Mediator=='perWorked',"% Probs. Worked","% Correct"),
    Outcome=ifelse(Outcome=='postS','Posttest','State Test'))%>%
  rename(
    "R<sup>2</sup><sub>Y&#126;D|X</sub>"="r2yd.x",
    "RV<sub>q = 1</sub>"=rv_q,
    "RV<sub>q = 1, &alpha; = 0.05</sub>"=rv_qa
    )%>%
  kable(digits=2,escape=FALSE,row.names=FALSE)
sink()

sink('tables/benchmark.html')
map_dfr(sensitivity,boundsTable)%>% kable(digits=2,escape=FALSE,row.names=FALSE)
sink()


save(atePost0,atePost,ateState0,ateState,
     ateWork0,ateWork,ateCorr0,ateCorr,
     sdPerWorked,effWorkedPost,effWorkedState,
     sdPerCorr,effCorrPost,effCorrState,
     workPostInd, workPostDir, workStateInd, workStateDir,
     corrPostInd, corrPostDir, corrStateInd, corrStateDir,
     irt,irtProb,
     file='results/forText.RData')
