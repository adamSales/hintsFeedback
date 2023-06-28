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


dat%>%
  mutate(Z=as.factor(Z))%>%
    filter(hasPosttest)%>%
    mutate(race=substr(as.character(race),1,1))%>%
  balPlot(covNames,trtVar="Z",data=.,top="Posttest Sample")

### wait what's with the 35 students with Scale_Score5=725?

dat%>%
  mutate(Z=as.factor(Z))%>%
    filter(hasStatetest)%>%
    mutate(race=substr(as.character(race),1,1))%>%
  balPlot(covNames,trtVar="Z",data=.,top="State Test Sample")

dat%>%
mutate(Z=as.factor(Z))%>%
  filter(hasBothtest)%>%
    mutate(race=substr(as.character(race),1,1))%>%
  balPlot(covNames,trtVar="Z",data=.,top="Intersection Sample")





bals <-
  map(c('Post','State','Both')%>%setNames(.,.),
      function(test)
        xBalance(covForm,
                 data=dat[dat[[paste0('has',test,'test')]],],
                 report=c('std.diffs','z.scores','chisquare.test'),
                 strata=list(cls=~class)))

balsMed <-
    map(c(paste0('hasPerCorr',c('','Post','State')),'hasAll')%>%setNames(.,.),
      function(test)
        xBalance(covForm,
                 data=dat[dat[[test]],],
                 report=c('std.diffs','z.scores','chisquare.test'),
                 strata=list(cls=~class))
      )


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
    c(paste0('has',c('Post','State','Both'),'test'),
      paste0('hasPerCorr',c('','Post','State')),
      'hasAll')){
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

lapply(diffs[1:3],
       function(x)
         paste0(round(x,3),
                ifelse(abs(x)>0.05,'*','')))%>%
  as.data.frame(row.names=rownames(diffs))%>%
  knitr::kable(caption="Table 5: Cox indices (for binary or categorical covariates) and Hedges's g (for numeric covariates) comparing baseline covariate means between each of the experimental conditions and the Active Control (positive differences indicate higher means for the experimental conditions, compared to Active Control). Stars indicate effect sizes >0.05, for which WWC recommends statistical adjustment",
        digits=3)


diffs[1:3]%>%rownames_to_column('covr')%>%pivot_longer(-covr,names_to = "Subset",values_to = "Effect Size")%>%mutate(covr=ifelse(covr%in%c('Asian','Other'),paste('race:',covr),covr))%>%ggplot(aes(`Effect Size`,covr,color=Subset))+geom_point()+geom_vline(xintercept=0)+geom_vline(xintercept=c(-0.25,-0.05,0.05,0.25),linetype='dashed')+ylab(NULL)

diffs[4:7]%>%rownames_to_column('covr')%>%pivot_longer(-covr,names_to = "Subset",values_to = "Effect Size")%>%mutate(covr=ifelse(covr%in%c('Asian','Other'),paste('race:',covr),covr))%>%ggplot(aes(`Effect Size`,covr,color=Subset))+geom_point()+geom_vline(xintercept=0)+geom_vline(xintercept=c(-0.25,-0.05,0.05,0.25),linetype='dashed')+ylab(NULL)


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

map_dfr(names(balsMed),
        function(nn)
          as.data.frame(RItools:::prepareXbalForPlot(balsMed[[nn]]))%>%
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

### acc. to WWC methodology?
adj=map(diffs,~rownames(diffs)[abs(.)>0.05])
adj=map(adj,~unique(sub("Hispanic/Latino|Asian|Other","race",.)))

### overall p-values
sapply(bals,function(x) x$overall)
sapply(balsMed,function(x) x$overall)

###########################################
####### Table 1
###########################################
tab1=map(c('Post','State'),#,'Both'),
         function(test){
           t1=table1(~Gender+race+accelerated+as.logical(EIP)+
                       as.logical(GIFTED)+as.logical(IEP)+as.logical(virtual)|Z,
                     data=dat[dat[[paste0('has',test,'test')]],])%>%
             as.data.frame()
           names(t1)[1]='varb'
           t1$varb=gsub(" |as.logical\\(|\\)","",t1$varb)
           t1=subset(t1,varb!='No')
           drop=NULL
           for(i in 1:nrow(t1))
             if(grepl("Yes",t1$varb[i])){ #t1$varb[i]=='Yes'){
               t1$varb[i]=t1$varb[i-1]
               drop=c(drop,i-1)
             }
           t1=t1[-drop,-4]
           if(test!="Post") t1=t1[,-1]
           t1
         })
tab1=do.call("cbind",tab1)

print(xtable(tab1),type='html',file='table1.html',include.rownames=FALSE)
