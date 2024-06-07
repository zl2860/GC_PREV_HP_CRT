#loading packages
library(data.table)
library(dplyr)
library(survival)
library(tableone)
library(eoffice)
library(forestplot)
library(epicalc)
library(ggplot2)
library(survminer)
library(broom)
library(readr)    
library(readOffice)
library(officer)
library(readxl)


# get the data
dat<-fread("E:/mylaptop/0myproject/my project/large_trial_analysis.csv",header=T)
dat$A5B5C<- factor(dat$A5B5C,levels = c(0,1,2),labels = c('Group B','Group A','Group C'))
dat$A5B5_cat<-factor(dat$A5B5_cat,levels = c(0,1,2,3),labels = c('Group B','Successful','Failed','Missing'))
dat$Gender<-factor(dat$Gender,levels = c(1,2),labels = c('Male','Female'))

# No of participants & cases
## incidence
table(dat$A5B5C)
table(A5B5_cat)
table(dat$A5B5C,dat$Y_GC_20221231)
table(dat$A5B5_cat,dat$Y_GC_20221231)
## mortality
table(dat$A5B5C,dat$Y_GC_death_20221231)
table(dat$A5B5_cat,dat$Y_GC_death_20221231)

# caculate pearson years
## incidence
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$A5B5C),FUN=sum,na.rm=T)
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$A5B5_cat),sum,na.rm=T)
## mortality
aggregate(dat$Surv_OS_total_20221231/365.25,by=list(type=dat$A5B5C),FUN=sum,na.rm=T)
aggregate(dat$Surv_OS_total_2022123/365.25,by=list(type=dat$A5B5_cat),sum,na.rm=T)
# fit models
## incidence
m1<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~factor(A5B5C)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach+howmoney_cat+worktype+schoolage1+howpeople1,data=dat)
m2<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach+howmoney_cat+worktype+schoolage1+howpeople1,data=dat)
## mortality
m3<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~factor(A5B5C)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach+howmoney_cat+worktype+schoolage1+howpeople1,data=dat)
m4<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach+howmoney_cat+worktype+schoolage1+howpeople1,data=dat)

summary(m1);summary(m2)
summary(m3);summary(m4)
