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
dat$Age_3cat<-factor(dat$Age_3cat)
dat$Alcohol<-factor(dat$Alcohol)
dat$Smoking<-factor(dat$Smoking)
dat$History_Cancer<-factor(dat$History_Cancer)
dat$History_Stomach<-factor(dat$History_Stomach)

#caculate No of participants & cases
## Final database
table(dat$A5B5C)
table(A5B5_cat)
table(dat$A5B5C,dat$Y_GC_20221231)
table(dat$A5B5_cat,dat$Y_GC_20221231)
## Prior locked database
table(dat$A5B5C,dat$Y_GC_20221231_old)
table(dat$A5B5_cat,dat$Y_GC_20221231_old)


# caculate pearson years
## Final database
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$A5B5C),FUN=sum,na.rm=T)
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$A5B5_cat),sum,na.rm=T)
## Prior locked database
aggregate(dat$Surv_case_GC_20221231_old/365.25,by=list(type=dat$A5B5C),FUN=sum,na.rm=T)
aggregate(dat$Surv_case_GC_20221231_old/365.25,by=list(type=dat$A5B5_cat),sum,na.rm=T)

# go through the models

## Final database
m1<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m2<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m1)
summary(m2)
## Prior locked database
m1<-coxph(Surv(Surv_case_GC_20221231_old,Y_GC_20221231_old)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m2<-coxph(Surv(Surv_case_GC_20221231_old,Y_GC_20221231_old)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)

summary(m3)
summary(m4)


# output
write_csv(tidy(m1),file = "STable1.csv" , col_names = TRUE)
write_csv(tidy(m2),file = "STable1.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m3),file = "STable1.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m4),file = "STable1.csv" , col_names = TRUE, append = TRUE)
