balPlot <- function(x,trtVar='Z',data,...){
    if(inherits(x,'formula')){
        ddd <- model.frame(x,data=data,na.action=na.pass)
        if(length(x)>2){
            if(names(ddd)[1]!=trtVar) warning(paste("Setting trtVar to",names(ddd)[1]))
            trtVar <- names(ddd)[1]
        } else ddd[[trtVar]] <- data[[trtVar]]
    } else if(inherits(x,'character')){
        ddd <- data[,c(x,trtVar)]
    } else{
        ddd <- x
        if(!trtVar%in%names(ddd)) ddd[[trtVar]] <- data[[trtVar]]
    }

    names(ddd)[names(ddd)==trtVar] <- 'Z'

    ddd <- ddd[!is.na(ddd$Z),]

   require(gridExtra)


    plots <- list()
    for(cc in setdiff(names(ddd),'Z')){
        ddd$cc <- ddd[[cc]]
        if(all(unique(na.omit(ddd$cc))%in%c(0,1))){
            plots[[cc]] <-
                ddd%>%group_by(Z)%>%summarize(Percent=mean(cc,na.rm=TRUE))%>%
                ggplot(aes(Z,Percent))+
                geom_col()+
                scale_y_continuous(labels=scales::percent)
        }
        else if(is.factor(ddd$cc)|is.character(ddd$cc)|n_distinct(ddd$cc)<7){
            plots[[cc]] <- ggplot(ddd,aes(x=Z,fill=cc))+geom_bar(position='fill')+
                scale_y_continuous(labels=scales::percent)
        }
        else{
            plots[[cc]] <- ggplot(ddd,aes(Z,cc))+geom_jitter(alpha=0.5)+geom_boxplot(outlier.shape=NA)
        }
        plots[[cc]] <- plots[[cc]]+    xlab(NULL)+ylab(NULL)+
            ggtitle(cc)+  theme(axis.text.x = element_text(angle = 45, hjust=1))
    }
    grid.arrange(arrangeGrob(grobs=plots),...)
}


balTestOne <- function(x,Z,cls){
    require(broom)
    keep <- !is.na(x)&!is.na(Z)
    x <- x[keep]
    Z <- Z[keep]
    if(!missing(cls)) cls <- cls[keep]
    if(all(unique(na.omit(x))%in%c(0,1)))
        return(
	prop.test(tapply(x,Z,sum),tapply(!is.na(x),Z,sum))%>%
	tidy()%>%mutate(method='prop.test'))
    if(is.factor(x)|is.character(x))
        return(chisq.test(x,Z)%>%tidy()%>%mutate(method='chisq.test'))

    mod <- if(!missing(cls)) lm(x~cls+Z) else lm(x~Z)
    mod%>%anova()%>%tidy()%>%filter(term=='Z')%>%
        mutate(method='ANOVA')%>%
        return()
}


xbalMult <- function(x,data,trtLevs,strata=list(NULL),trtVar='Z',...){
    nlevs <- length(trtLevs)
    ntest <- nlevs*(nlevs-1)/2

    if(trtVar!='Z') data$Z <- data[[trtVar]]

    bals <- list()
    for(i in 1:(nlevs-1))
        for(j in (i+1):nlevs){
            ddd <- data[data[[trtVar]]%in%trtLevs[c(i,j)],]
            ddd[[trtVar]] <- ddd[[trtVar]]==trtLevs[i]
            bals[[paste(trtLevs[i],'vs.',trtLevs[[j]])]] <-
                xBalance(x,strata=strata,data=ddd,
                            report=c('std.diffs','z.scores','chisquare.test'),
                            ...)
        }
    ## par(mfrow=c(1,ntest))
    ## walk(names(bals),~plot(bals[[.]],main=.))
#    par(mfrow=c(1,1))
    return(bals)
}

plotXbals <- function(bals){
    require(gridExtra)
    plots <- map(1:length(bals),
                 ~plot(bals[[.x]],ggplot=TRUE)+
                 ggtitle(names(bals)[.x])+
                 geom_vline(xintercept=c(-.25,-.05,.05,.25),linetype='dotted')+
                     guides(color=FALSE,fill=FALSE,shape=FALSE))
    grid.arrange(arrangeGrob(grobs=plots))
}


plotWWC=function(ov,diff,labs,wwc,...){
  if(missing(wwc)) wwc=read.csv("data/wwc.csv")
  names(wwc)[1] <- "Overall"


  with(wwc,
  plot(Overall,Differential1,type="l",ylim=c(0,11),
       ...,
       xlab="Overall Attrition", ylab="Differential Attrition"))
  polygon(c(0,0,65,65),c(0,11,11,0),col="red")
  polygon(c(0,wwc[[1]],65),c(0,wwc$Differential1,0),col="yellow")
  polygon(c(0,wwc[[1]]),c(0,wwc$Differential0),col="green")

  if(all(ov<1)) ov <- ov*100
  diff <- abs(diff)
  if(all(diff< 1)) diff <- diff*100

  points(ov,diff,pch=16)
  text(ov,diff,labs,pos=2)
}


### diagnostic plots
diagPlots <- function(mod){
  op=par(ask=TRUE)
  mf=model.frame(mod)
  mmAll <- model.matrix(mod)
  diagnosticAll <-
    data.frame(x=fitted(mod),y=resid(mod),nn='main')

  for(i in 2:ncol(mmAll))
    if(n_distinct(mmAll[,i])>2){
      mod1=lm(mf[,1]~mmAll[,-i])
      mod2=lm(mmAll[,i]~mmAll[,-i])
      diagnosticAll <-
        bind_rows(diagnosticAll,
                data.frame(x=resid(mod2),y=resid(mod1),
                           nn=colnames(mmAll)[i]))
    }

  print(
    diagnosticAll%>%
    filter(nn!="main")%>%
    ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
    geom_hline(yintercept=0)+
    facet_wrap(~nn,scales="free")
 )


 print(
   diagnosticAll%>%
   filter(nn=="main")%>%
   ggplot(aes(x,y))+geom_jitter()+geom_smooth(se=FALSE)+
   geom_hline(yintercept=0)+
   labs(x="residuals",y="fitted values")
 )

  if(n_distinct(mf[,1])<15)
    arm::binnedplot(fitted(mod),resid(mod))

  mf$Y <- mf[,1]
  print(
    mf%>%
    mutate(Z=as.factor(Z))%>%
    group_by(pretestC,Z)%>%
    summarize(Y=mean(Y,na.rm=TRUE),n=n())%>%
    ggplot(aes(pretestC,Y,color=Z))+
    geom_point(aes(size=n))+
    geom_smooth()
  )
  par(op)
}


nonLinPlot <- function(mod,x){
  newdat <- model.frame(mod)[rep(1,100),]
  newdat <- newdat[,-grep(x,names(newdat))]
  newdat[[x]] <- seq(0,1,length=100)
  newdat <- rbind(within(newdat,Z <- 1),within(newdat,Z <- 0))
  mm <- model.matrix(formula(mod),data=newdat)
  cols <- grep(x,colnames(mm))
  pred <- mm[,cols]%*%cbind(coef(mod)[cols])+coef(mod)["Z"]*mm[,"Z"]
  sepred <- sqrt(diag(mm[,cols]%*%vcov(mod)[cols,cols]%*%t(mm[,cols])))

  yhat <- data.frame(xx=newdat[[x]],Z=factor(newdat$Z),
                     fit=pred,lwr=pred-2*sepred,upr=pred+2*sepred)
  yhat%>%ggplot(aes(xx,fit,color=Z,fill=Z,group=Z,ymin=lwr,ymax=upr))+
    geom_line()+geom_ribbon(alpha=0.2)+xlab(x)
}
