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
dat$Alcohol<-factor(dat$Alcohol)
dat$Smoking<-factor(dat$Smoking)
dat$History_Cancer<-factor(dat$History_Cancer)
dat$History_Stomach<-factor(dat$History_Stomach)
dat$Age_3cat<-factor(dat$Age_3cat)
dat$schoolage1<-ifelse(dat$schoolage==0 | dat$schoolage== 1,1,
                       ifelse(dat$schoolage==2,2,ifelse(dat$schoolage==3,3,99)))
dat$schoolage1<-factor(dat$schoolage1,
                        levels = c(1,2,3,99),
                        labels = c('Primary school or above','Middle school','Junior college or above','Missing'))#未正式上过学，小学，中学(初高中）

dat$worktype<-factor(dat$worktype,levels = c(0,1,2,3,99),labels = c('Farmer','Local workers','Migrant workers','Other','Missing'))
dat$howmoney_cat<-ifelse(dat$howmoney == 99,99,
                          ifelse(dat$howmoney>=3000 & dat$howmoney <10000,1,
                                 ifelse(dat$howmoney >=10000,2,NA)))#按照收入是否超过1w分类
dat$howmoney_cat<-factor(dat$howmoney_cat)
dat$howpeople1<-ifelse(dat$howpeople >=1 & dat$howpeople <=3,1,
                        ifelse(dat$howpeople ==4,2,
                               ifelse(dat$howpeople >=5 & dat$howpeople <99,3,99)))
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