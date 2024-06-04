

psd=function(y,z){
  z=z[!is.na(y)]
  y=y[!is.na(y)]
  z=as.numeric(z)
  n1=sum(z==1)
  n0=sum(z!=1)
  v1=var(y[z==1])
  v0=var(y[z!=1])
  sqrt(((n1-1)*v1+(n0-1)*v0)/(n1+n0-2))
}

Scale <- function(x,z=NULL){
  scl <- if(is.null(z)) sd(x,na.rm=TRUE) else psd(x,z)
  (x-mean(x,na.rm=TRUE))/scl
}

dat0 <- read_csv('data/assess_student.csv')%>%
  filter(condition_assignment%in%c("Delay","Instant"))%>%
  select(StuID,condition_assignment,starts_with("pre_"),Scale_Score5,Performance_Level5,
         post_total_math_score, Scale_Score7)%>%
  left_join(read_csv('data/student_demo.csv')%>%select(-1),by="StuID")%>%
  left_join(read_csv('data/student_roster.csv')%>%
            select("StuID",ends_with("IDPre"),TeaIDEnd,ClaIDEnd,'virtual','courseName','DROPSCH1','DROPSCH2'),
            by='StuID')


### attrition variables
dat1 <- dat0%>%
  mutate(
    hasStatetest=is.finite(Scale_Score7),
    hasPretest=is.finite(pre_total_math_score),
    hasPosttest=is.finite(post_total_math_score),
    hasBothtest=hasStatetest&hasPosttest,
    race=race_ethnicity%>%
               factor()%>%
               fct_lump_min(100)%>%
               fct_recode(`Hispanic/Latino`="1",Asian="3",White="6")%>%
      fct_relevel('White'),
    pretestC = pre_total_math_score-round(mean(pre_total_math_score,na.rm=TRUE)),
    pretestMiss=is.na(pre_total_math_score),
    pretestC=ifelse(is.na(pretestC),0,pretestC),
    accelerated=grepl('Accelerated',courseName),
    teach=ifelse(is.na(TeaIDPre),TeaIDEnd,TeaIDPre),
    class=as.factor(ifelse(is.na(ClaIDPre),ClaIDEnd,ClaIDPre)),
    across(c(Scale_Score5,pre_MA_total_score,pre_MSE_total_score,pre_PS_tasks_total_score,pre_avg_time_on_tasks),~ifelse(is.na(.),mean(.,na.rm=TRUE),.),.names="{.col}imp"),
#    ScaleScore5imp=ifelse(is.na(Scale_Score5),mean(Scale_Score5,na.rm=TRUE),Scale_Score5),
    ScaleScore5miss=ifelse(is.na(Scale_Score5),1,0),
    Z=ifelse(condition_assignment=="Instant",1,0),
    logTime=log(pre_avg_time_on_tasksimp)
  )
dat1$race[is.na(dat1$race)] <- "Other"


## ## drop the school that has no pretest scores
## ## which school?
## noPre=dat1%>%
##   group_by(SchIDPre)%>%
##   summarize(pretest=mean(hasPretest))%>%
##   filter(pretest<0.01)%>%
##   pull(SchIDPre)

## ### Which teachers?
## noPreTch=unique(dat1$TeaIDPre[dat1$SchIDPre==noPre])

## ### update: we are keeping them in (as we should)
## dat2 <- dat1%>%filter(!teach%in%noPreTch)

### keep the same students as impact paper
dat2 <- filter(dat1,DROPSCH1==0)
dat3 <- filter(dat2,DROPSCH2==0)

dat4=filter(dat3,hasPretest)#hasPosttest|hasStatetest)

#datS=dat4%>%filter(hasPretest)

datS=mutate(dat4,
           Scale_Score7S = Scale(Scale_Score7,Z),
           postS=Scale(post_total_math_score,Z))

#save(dat,file='data/feedbackData.RData')

#######################################
## correctness and completion
#######################################

actDat <- read_csv('data/assist_student_problem.csv')%>%
  filter(graded==1)%>%
  mutate(
    prob_part_id=paste0(problem_id,'_',problem_part)
  )%>%
  rename(firstTry=correct_response_first_attempt_before_hint)%>%
  group_by(prob_part_id)%>%
  mutate(
    nTotal=n(),
    nStudComplete=sum(!is.na(firstTry)),
    probPercCorrect=sum(firstTry,na.rm=TRUE)/nTotal
    )


actDat%>%group_by(prob_part_id)%>%
  summarize(across(c(nTotal,nStudComplete,probPercCorrect),~.[1]))%>%
  ungroup()%>%map(summary)

dat <-
  actDat%>%
  ungroup()%>%
  mutate(num_prob=n_distinct(prob_part_id))%>%
  group_by(StuID)%>%
  summarize(
    nworked=n(),
    ncomp=sum(!is.na(firstTry)),
    perCorr=mean(firstTry,na.rm=TRUE)*100,
    num_prob=num_prob[1])%>%
  right_join(datS,by="StuID")%>%
  mutate(
    num_prob=mean(num_prob,na.rm=TRUE),
    nworked=ifelse(is.na(nworked),0,nworked),
    ncomp=ifelse(is.na(ncomp),0,ncomp),
    perWorked=100*nworked/num_prob,
    perComp=ncomp/num_prob*100,
    logitWorked=qlogis((nworked+1)/(num_prob+2)),
    logitComp=qlogis((ncomp+1)/(num_prob+2)),
    hasPerCorr=!is.na(perCorr),
    hasPerCorrPost=hasPerCorr&hasPosttest,
    hasPerCorrState=hasPerCorr&hasStatetest,
    hasAll=hasPerCorr&hasBothtest
    )

#dat <- dat%>%group_by(class)%>%
 # mutate(pz=mean(Z))%>%ungroup()%>%filter(pz<1,pz>0)%>%mutate(class=as.factor(class))


save(dat,file='data/feedbackData.RData')


#######################################
## attrition analysis
#######################################

### analysis sample vs n randomized
nRandomized=table(dat2$Z)
att=rbind(
  cbind(
    post=1-sum(~hasPosttest|Z,data=dat)/nRandomized,
   state=1-sum(~hasStatetest|Z,data=dat)/nRandomized,
#   bothtest=1-sum(~hasBothtest|Z,data=dat)/nRandomized,
  perCorr=1-sum(~hasPerCorr|Z,data=dat)/nRandomized),
#  perCorrPost=1-sum(~hasPerCorrPost|Z,data=dat)/nRandomized,
#  perCorrState=1-sum(~hasPerCorrState|Z,data=dat)/nRandomized),#,
#  perCorrBoth=1-sum(~hasAll|Z,data=dat)/nRandomized),
all=1-sapply(select(dat,
                    hasPosttest,
                    hasStatetest,
                 #   hasBothtest,
                    hasPerCorr#,
                    #hasPerCorrPost,
                                        #hasPerCorrState
                    ),
                                        #hasAll),
             sum)/sum(nRandomized))

labs <- c(
  post="Posttest",
  state="State Test",
  perCorr="Accuracy")#,
#  perCorrPost="Accuracy & Posttest",
#  perCorrState="Accuracy & State Test")



## png('figure/wwcPlotAll.png')
## plotWWC(ov=att['all',-4],diff=apply(att[,-4],2,function(x) x['1']-x['0']),
##         labs=labs[colnames(att)[-4]],main="Attrition for Delayed vs Immediate Feedback")
## points(att['all',4]*100,abs(att[1,4]-att[2,4])*100,pch=16)
## text(att['all',4]*100,abs(att[1,4]-att[2,4])*100,labs[4],pos=1)
## dev.off()


png('figure/wwcPlotAll.png')
plotWWC(ov=att['all',],diff=apply(att[,],2,function(x) x['1']-x['0']),
        labs=labs[colnames(att)],main="Attrition for Delayed vs Immediate Feedback")
dev.off()


rownames(att) <- c('Delayed','Immediate','Overall')
colnames(att) <- labs[colnames(att)]
att <- rbind(att,'|Differential|'=abs(att[1,]-att[2,]))

sink('tables/attritionTable.html')
kable(att,format='html',digits=3)
sink()
