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
dat$ABC<-ifelse(dat$A5B5C=='Group C',2,
                ifelse(dat$AB2==0,0,
                       ifelse(dat$AB2==1,1,NA)))
dat$ABC<-factor(dat$ABC,levels = c(0,1,2),labels = c('Group B','Group A','Group C'))
dat$AB_cat<-factor(dat$AB_cat,levels = c(0,1,2,3),labels = c('Group B','Successful','Failed','Missing'))
dat$Gender<-factor(dat$Gender,levels = c(1,2),labels = c('Male','Female'))
dat$Alcohol<-factor(dat$Alcohol)
dat$Smoking<-factor(dat$Smoking)
dat$History_Cancer<-factor(dat$History_Cancer)
dat$History_Stomach<-factor(dat$History_Stomach)

#caculate No of participants & cases
##ITT
table(dat$A5B5C)
table(A5B5_cat)
table(dat$A5B5C,dat$Y_GC_20221231)
table(dat$A5B5_cat,dat$Y_GC_20221231)
table(dat$A5B5C,dat$Y_GC_death_20221231)
table(dat$A5B5_cat,dat$Y_GC_death_20221231)

##PP
table(dat$ABC)
table(dat$AB_cat)
table(dat$ABC,dat$Y_GC_20221231)
table(dat$AB_cat,dat$Y_GC_20221231)
table(dat$ABC,dat$Y_GC_death_20221231)
table(dat$AB_cat,dat$Y_GC_death_20221231)

# caculate pearson years
##ITT 
###incidence
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$A5B5C),FUN=sum,na.rm=T)
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$A5B5_cat),sum,na.rm=T)

## ITT 
###mortality
aggregate(dat$Surv_OS_total_20221231/365.25,by=list(type=dat$A5B5C),FUN=sum,na.rm=T)
aggregate(dat$Surv_OS_total_2022123/365.25,by=list(type=dat$A5B5_cat),sum,na.rm=T)


## PP
###incidence
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$ABC),sum,na.rm=T)
aggregate(dat$Surv_case_GC_20221231/365.25,by=list(type=dat$AB_cat),sum,na.rm=T)
##PP
###mortality
aggregate(dat$Surv_OS_total_20221231/365.25,by=list(type=dat$ABC),FUN=sum,na.rm=T)
aggregate(dat$Surv_OS_total_20221231/365.25,by=list(type=dat$AB_cat),sum,na.rm=T)


# go through the models
##ITT 
###incidence
m1<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m2<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
###mortality
m3<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m4<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

##PP
###incidence
m5<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~ABC+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m6<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~AB_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
###mortality
m7<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~ABC+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m8<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~AB_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m5)
summary(m6)
summary(m7)
summary(m8)


#output
write_csv(tidy(m1),file = "table2.csv" , col_names = TRUE)
write_csv(tidy(m2),file = "table2.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m3),file = "table2.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m4),file = "table2.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m5),file = "table2.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m6),file = "table2.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m7),file = "table2.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m8),file = "table2.csv" , col_names = TRUE, append = TRUE)
rm(m1);rm(m2);rm(m3);rm(m4);rm(m5);rm(m6);rm(m7);rm(m8)
