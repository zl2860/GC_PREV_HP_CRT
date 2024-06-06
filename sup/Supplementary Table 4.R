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


dff<-dat %>% filter(Missed_dose==2)
# incidence
## No. of participants & cases
table(dff$A5B5C)
table(dff$A5B5_cat)
table(dff$A5B5C,dff$Y_GC_20221231)
table(dff$A5B5_cat,dff$Y_GC_20221231)
## Person years
aggregate(dff$Surv_case_GC_20221231/365.25,by=list(type=dff$A5B5C),FUN=sum,na.rm=T)
aggregate(dff$Surv_case_GC_20221231/365.25,by=list(type=dff$A5B5_cat),sum,na.rm=T)

## fit cox model
m3<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dff)
m4<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dff)
summary(m3)
summary(m4)

# mortality
## No. of participants & cases
table(dff$A5B5_cat)
table(dff$A5B5C)
table(dff$A5B5C,dff$Y_GC_death_20221231)
table(dff$A5B5_cat,dff$Y_GC_death_20221231)
### Person years
aggregate(dff$Surv_OS_total_20221231/365.25,by=list(type=A5B5C),sum,na.rm=T)
aggregate(dff$Surv_OS_total_20221231/365.25,by=list(type=A5B5_cat),sum,na.rm=T)

### fit cox model
m1<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dff)
m2<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dff)
summary(m1)
summary(m2)

