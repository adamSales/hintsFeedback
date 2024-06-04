library(knitr)
library(rmarkdown)
library(tidyverse)
library(RItools)
library(mosaic)
library(clubSandwich)
library(texreg)
library(table1)
library(xtable)
library(lmtest)
library(estimatr)
library(stargazer)
library(sandwich)
library(loop.estimator)
library(VennDiagram)
library(mediation)
library(mirt)
library(kableExtra)
library(mosaic)
library(sensemakr)

#library(tableone)
fround <- function(x,digits=3) format(round(x,digits),nsmall=digits)
select <- dplyr::select

stars <- function(p)
  ifelse(p<0.001,'***',
  ifelse(p<0.01,'**',
  ifelse(p<0.05,'*',
  ifelse(p<0.1,'.',''))))

nlist <- function(...){
  if(length(names(list(...)))) return(list(...))
  nms <- as.character(match.call())[-1]
  out <- list(...)
  names(out) <- nms
  out
}

prtci <- function(ci,digits=2,paren=TRUE)
  paste0(ifelse(paren,'(','['),
         sprintf(paste0("%.",digits,"f"),ci[1]),
         ',',
         sprintf(paste0("%.",digits,"f"),ci[2]),
         ifelse(paren,')',']'))

## helper functions
source("code/functions.r") ### functions for covariate balance plots and diagonistic plots
source("code/med_diagram.r")


##### clean data
source("code/feedbackData.r")

covNames <-
  c("pretestC","Scale_Score5imp","ScaleScore5miss","race","MALE","EIP","ESOL","IEP","GIFTED","accelerated","virtual","logTime","pre_MA_total_scoreimp","pre_MSE_total_scoreimp","pre_PS_tasks_total_scoreimp")

covForm <- as.formula(paste("I(Z==1)~",paste(covNames,collapse="+")))

## Covariate means (Table 1) and covariate balance results
source('code/attritionCovariates.r')

## Main analyses for RQ1 and RQ2, and regressions for RQ3
source('code/regressions.r')

## supplementary Rasch analysis (this takes a long time)
source('code/irt.r')

## Mediation analysis
source('code/mediation.r')

## sensitivity analysis
source('code/sensitivity.r')

## Make results tables (and a figure, too)
source('code/tables.r')

## print out the text of the results section
render('results.rmd')


sink('results/sessionInfo.txt')
sessionInfo()
sink()
