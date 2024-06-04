### sample sizes
s=data.frame(
  test=c('post','state','both','total'),
  n=c(
    sapply(select(dat,hasPosttest,hasStatetest,hasBothtest),sum),
    with(dat, sum(hasPosttest|hasStatetest))),
  trt=c(
    sapply(dat%>%filter(Z==1)%>%select(hasPosttest,hasStatetest,hasBothtest),sum),
    with(filter(dat,Z==1), sum(hasPosttest|hasStatetest))),
   ctl=c(
    sapply(dat%>%filter(Z==0)%>%select(hasPosttest,hasStatetest,hasBothtest),sum),
    with(filter(dat,Z==0), sum(hasPosttest|hasStatetest)))
   )


venn.diagram(list(Posttest=which(dat$hasPosttest),`State Test`=which(dat$hasStatetest),#`% Worked`=seq(nrow(dat)),
                  `Accuracy`=which(dat$hasPerCorr)),#'venn4.tiff')
             cat.pos=c(-40,40,180),
             filename='venn.tiff')



## ### plot % observed
## dat%>%
##   bind_rows(mutate(dat,Z='All'))%>%
##   group_by(Z)%>%
##   summarize(across(starts_with('has'),mean))%>%
##   select(-hasPretest)%>%
##   pivot_longer(-Z,names_to='Outcome',names_prefix='has',values_to="perObs")%>%
##   ungroup()%>%
##   ggplot(aes(Outcome,perObs,color=Z,group=Z))+geom_point()+geom_line()

### plot overall & differential attrition vs WWC standards


#### covariate balance
covNamesBal <- c("Pretest","State Test 5th Gr.","Missing 5th Gr.","race","MALE","EIP","ESOL","IEP","GIFTED","accelerated","virtual","log(Pretest Time)","Math Anx.","Math Self Eff.","Perc. Sens.")

renameFunc <- function(x)
  rename(x,
         "Pretest"=pretestC,
         "Math Anx."=pre_MA_total_scoreimp,
         "Math Self Eff."=pre_MSE_total_scoreimp,
         "Perc. Sens."="pre_PS_tasks_total_scoreimp",
         "State Test 5th Gr."=Scale_Score5imp,
         "Missing 5th Gr."=ScaleScore5miss,
         "log(Pretest Time)"=logTime)

dat%>%
  mutate(Z=ifelse(Z==1,'I','D'))%>%
  renameFunc()%>%
    filter(hasPosttest)%>%
    mutate(race=substr(as.character(race),1,1))%>%
  balPlot(covNamesBal,trtVar="Z",data=.,top="Posttest Sample")

### wait what's with the 35 students with Scale_Score5=725?

dat%>%
  mutate(Z=ifelse(Z==1,'I','D'))%>%
  renameFunc()%>%
  filter(hasStatetest)%>%
  mutate(race=substr(as.character(race),1,1))%>%
  balPlot(covNamesBal,trtVar="Z",data=.,top="State Test Sample")

## dat%>%
## mutate(Z=as.factor(Z))%>%
##   filter(hasBothtest)%>%
##     mutate(race=substr(as.character(race),1,1))%>%
##   balPlot(covNames,trtVar="Z",data=.,top="Intersection Sample")





bals <-
  map(c('hasPosttest','hasStatetest','hasPerCorr')%>%setNames(.,.),
      function(test)
        xBalance(covForm,
                 data=dat[dat[[test]],],
                 report=c('std.diffs','z.scores','chisquare.test')))#,
#                 strata=list(cls=~class)))

balsMed <-
    map(c(paste0('hasPerCorr',c('','Post','State')))%>%setNames(.,.),
      function(test)
        xBalance(covForm,
                 data=dat[dat[[test]],],
                 report=c('std.diffs','z.scores','chisquare.test')))#,
#                 strata=list(cls=~class))
#      )


balTab <- #cbind(
  do.call("cbind",map(bals,~c(.$results[,'std.diff',1],"Overall p"=.$overall$p.value)))#,
 # c(balsMed$hasPerCorr$results[,'std.diff',1],"Overall p"=balsMed$hasPerCorr$overall$p.value)
#)

for(i in 1:length(covNames)) rownames(balTab)[which(rownames(balTab)==covNames[i])] <- covNamesBal[i]
rownames(balTab) <- gsub('race|TRUE','',rownames(balTab))
colnames(balTab) <- c('Posttest','State Test','Accuracy')

sink('tables/balanceTab.html')
kable(balTab,digits=2,format='html')
sink()

###################
### estimating imbalance as suggested in WWC document
dCox=function(x,z){
  pi=mean(x[z],na.rm=TRUE)
  pc=mean(x[!z],na.rm=TRUE)
  (qlogis(pi)-qlogis(pc))/1.65
}

hedgesG=function(x,z){
  yi=mean(x[z],na.rm=TRUE)
  yc=mean(x[!z],na.rm=TRUE)
  ni=sum(!is.na(x[z]))
  nc=sum(!is.na(x[!z]))
  N=ni+nc
  omega=1-3/(4*N-9)
  omega*(yi-yc)/sqrt(((ni-1)*var(x[z])+(nc-1)*var(x[!z]))/(N-2))
}

stdDiffs=list()
for(subst in
    c(paste0('has',c('Post','State'),'test'),
      'hasPerCorr')){
#      'hasAll')){
  diffDat=
    dat[dat[[subst]],]%>%
    mutate(Znum=Z==1)%>%
    select(all_of(covNames),Znum)

  bincovs=covNames[which(map_lgl(covNames,~n_distinct(dat[[.]],na.rm=TRUE)==2))]

  diffs=map_dbl(bincovs%>%setNames(.,.),
                ~dCox(diffDat[[.]],diffDat$Znum))

  for(r in levels(diffDat$race)[-1]){
    diffs=c(diffs,dCox(diffDat$race==r,diffDat$Znum))
    names(diffs)[length(diffs)] <- r
  }

  diffs=c(diffs,
          map_dbl(setdiff(covNames, c(bincovs,'race'))%>%setNames(.,.),
                  ~hedgesG(diffDat[[.]],diffDat$Znum)))


  stdDiffs[[subst]] <- diffs
}
diffs <- as.data.frame(stdDiffs)

for(i in 1:length(covNames)) rownames(diffs)[which(rownames(diffs)==covNames[i])] <- covNamesBal[i]
rownames(diffs) <- gsub('race|TRUE','',rownames(diffs))
colnames(diffs) <- c('Posttest','State Test','Accuracy')


sink('tables/covBalanceWWC.html')
diffs%>%
  knitr::kable(caption="Table 5: Cox indices (for binary or categorical covariates) and Hedges's g (for numeric covariates) comparing baseline covariate means between each of the experimental conditions and the Active Control (positive differences indicate higher means for the experimental conditions, compared to Active Control).",
               digits=2)%>%print()
sink()



diffs%>%#rownames_to_column('covr')%>%
  mutate(covr=factor(rownames(diffs),levels=rownames(diffs)[order(apply(diffs,1,min))]))%>%
  within(levels(covr) <- ifelse(levels(covr)%in%c('Asian','Other'),paste('race:',levels(covr)),levels(covr)))%>%
  pivot_longer(-covr,names_to = "Subset",values_to = "Effect Size")%>%ggplot(aes(`Effect Size`,covr,color=Subset))+geom_point()+geom_vline(xintercept=0)+geom_vline(xintercept=c(-0.25,-0.05,0.05,0.25),linetype='dashed')+ylab(NULL)
ggsave('figure/covBalWWC.png',width=6,height=7)


### overall balance p-value
sink('tables/omnibusBalance.html')
bals%>%map_dfr(~.$overall)%>%`rownames<-`(c('Posttest','State Test','Accuracy'))%>%
  kable(digits=2,format='html')%>%print()
sink()


#diffs[4:7]%>%rownames_to_column('covr')%>%pivot_longer(-covr,names_to = "Subset",values_to = "Effect Size")%>%mutate(covr=ifelse(covr%in%c('Asian','Other'),paste('race:',covr),covr))%>%ggplot(aes(`Effect Size`,covr,color=Subset))+geom_point()+geom_vline(xintercept=0)+geom_vline(xintercept=c(-0.25,-0.05,0.05,0.25),linetype='dashed')+ylab(NULL)


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

## map_dfr(names(balsMed),
##         function(nn)
##           as.data.frame(RItools:::prepareXbalForPlot(balsMed[[nn]]))%>%
##           rownames_to_column()%>%
##           mutate(test=nn))%>%
##   ggplot(aes(y = rowname, x = cls)) +
##   geom_vline(xintercept = 0) +
##   geom_vline(xintercept=c(-.25,-.05,.25,.05),linetype='dotted')+
##   geom_point() +
##   theme(legend.position = "bottom")+
##   facet_wrap(~test,nrow=1)+theme_bw()+xlab("Standardized Difference")+ylab(NULL)



### which variables are imbalanced with |stand. diff|>0.05?
bals%>%
  map(~rownames(.$results)[abs(.$results[,'std.diff',1])>0.05])%>%
  unlist()%>%unique()

### acc. to WWC methodology?
adj=map(diffs,~rownames(diffs)[abs(.)>0.05])
adj=map(adj,~unique(sub("Hispanic/Latino|Asian|Other","race",.)))

### overall p-values
sapply(bals,function(x) x$overall)
sapply(balsMed,function(x) x$overall)

###########################################
####### Table 1
###########################################
#tab1=#map(c('Post','State'),#,'Both'),

         #function(test){


## my.render.cont <- function(x) {
##     with(stats.apply.rounding(stats.default(x), digits=2), c("",
##         "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
## }

## keep traling 0s
my.render.cont <- function(x) c("", "Mean (SD)"=paste0(fround(mean(x,na.rm=TRUE),2),' (',fround(sd(x,na.rm=TRUE),2),')'))


t1=bind_rows(
  mutate(dat,subset="Full"),
  dat%>%filter(hasPosttest)%>%mutate(subset="Posttest"),
  dat%>%filter(hasStatetest)%>%mutate(subset="State Test"),
  dat%>%filter(hasPerCorr)%>%mutate(subset="Accuracy")
  )%>%
    mutate(across(c(EIP,GIFTED,IEP,virtual,Z), as.logical),
           Pretest=round(pretestC+5),
           subset=factor(subset,levels=unique(subset))
           )%>%
    rename(
      `5th Grade State Test`=Scale_Score5imp,
      `log(Pretest time)`= logTime,
        `Math Anxiety`=pre_MA_total_scoreimp,
      `Math Self-Eff`=pre_MSE_total_scoreimp,
      `Perceptual Sens.`=pre_PS_tasks_total_scoreimp,
      Posttest=post_total_math_score,
      `State Test`=Scale_Score7S,
      `% Practice Problems Worked`=perWorked,
      `% Practice Problems Correct`=perCorr)|>
    (\(ddd) table1(~Gender+race+accelerated+EIP+GIFTED+IEP+virtual+
                    Pretest+
      `5th Grade State Test`+
      `log(Pretest time)`+
        `Math Anxiety`+
      `Math Self-Eff`+
      `Perceptual Sens.`+
      Posttest+
      `State Test`+
      `% Practice Problems Worked`+
      `% Practice Problems Correct`|subset,#condition_assignment,
      overall=FALSE,
      render.continuous=my.render.cont,
      data=ddd))()

sink('tables/tableOne.html')
print(t1)
sink()
