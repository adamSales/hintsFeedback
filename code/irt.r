corrAll <- lm(perCorr ~ Z + pretestC + Scale_Score5imp + ScaleScore5miss +
    race + MALE + EIP + ESOL + IEP + GIFTED + accelerated + logTime +
    pre_MA_total_scoreimp + pre_MSE_total_scoreimp + pre_PS_tasks_total_scoreimp +
    pretestMiss + class,
    data=dat, subset=hasPerCorr)

actDat <- read_csv('data/assist_student_problem.csv')%>%
  filter(graded==1)%>%
  inner_join(select(dat,StuID,Z))%>%
  mutate(
    prob_part_id=paste0(problem_id,'_',problem_part)
  )%>%
  rename(firstTry=correct_response_first_attempt_before_hint)%>%
  group_by(prob_part_id,Z)%>%
  mutate(
    nTotal=n(),
    nStudComplete=sum(!is.na(firstTry)),
    probPercCorrect=mean(firstTry,na.rm=TRUE)
  )

### get rasch difficulty parameters
irtDat <- actDat%>%
  ungroup()%>%
  filter(probPercCorrect<1,
         probPercCorrect>0,
         nStudComplete>5)%>%
  pivot_wider(id_cols=StuID,names_from=prob_part_id,values_from=firstTry)

delayIrtDat <- irtDat%>%filter(StuID%in%dat$StuID[dat$Z==0])%>%select(-StuID)

rasch <- mirt.model("math= 2 - 306")
raschResults <- mirt(delayIrtDat,rasch,"Rasch")

raschDat2 <- actDat%>%
  ungroup()%>%
  filter(probPercCorrect<1,
         probPercCorrect>0,
         nStudComplete>5)%>%
  select(StuID,prob_part_id,firstTry)%>%
  inner_join(dat)

irtPars <- coef(raschResults,simplify = TRUE, IRTpar = TRUE)$items

raschDat2$difficulty <- irtPars[raschDat2$prob_part_id,'b']
raschDat2 <- filter(raschDat2,is.finite(difficulty))

raschEff2 <-  glm(
  update(formula(corrAll),firstTry~.),#+(1|StuID)),
  family=binomial,
  offset=-difficulty,
  data=raschDat2)

VCV <- vcovCR(raschEff2, raschDat2$StuID,"CR2")
raschCoefTest <- coef_test(raschEff2,vcov=VCV,coefs="Z")

delayDat <- model.frame(raschEff2)
delayDat$Z <- 0
difficulty <- -raschEff2$offset
delayPred <- predict(raschEff2,delayDat, type='response')

instDat <- model.frame(raschEff2)
instDat$Z <- 1
instPred <- predict(raschEff2,instDat, type='response')

probScale <- mean(instPred)-mean(delayPred)


save(raschEff2,VCV,raschCoefTest,probScale,file='results/raschEff2.RData')
