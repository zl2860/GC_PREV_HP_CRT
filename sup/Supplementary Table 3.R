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


# Exclusion of gastric cancers identified in the first two years 
## incidence
### No. of participants & cases
dff<-dat%>%filter(Y_GC_20221231_lag2== 1 | Y_GC_20221231_lag2 ==0 )

table(dff$A5B5C)
table(dff$A5B5_cat)

table(dat$A5B5C,dat$Y_GC_20221231_lag2)
table(dat$A5B5_cat,dat$Y_GC_20221231_lag2)

### Person years
aggregate(dff$Surv_case_GC_20221231/365.25,by=list(type=dff$A5B5C),FUN=sum,na.rm=T)
aggregate(dff$Surv_case_GC_20221231/365.25,by=list(type=dff$A5B5_cat),sum,na.rm=T)

### fit cox model
m5<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231_lag2)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m6<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231_lag2)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m5)
summary(m6)

## mortality
df1<-dat%>%filter(Y_GC_death_20221231_lag2== 1 | Y_GC_death_20221231_lag2 ==0 )
### No. of participants & cases

table(df1$A5B5_cat)
table(df1$A5B5C)
table(df1$A5B5C,df1$Y_GC_death_20221231_lag2)
table(df1$A5B5_cat,df1$Y_GC_death_20221231_lag2)

### Person years
aggregate(df1$Surv_OS_total_20221231/365.25,by=list(type=A5B5C),sum,na.rm=T)
aggregate(df1$Surv_OS_total_20221231/365.25,by=list(type=A5B5_cat),sum,na.rm=T)

### fit cox model
m7<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231_lag2)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=df1)
m8<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231_lag2)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=df1)

summary(m7)
summary(m8)


#Exclusion of all cancers identified in the first two years
## incidence
### No. of participants & cases
df1<-dat%>% filter(Y_incidence_lag2 != 1)#participants identified cancer in the first two years are excluded from the analysis
table(df1$A5B5_cat)
table(df1$A5B5C)

table(df1$A5B5C,df1$Y_GC_20221231_lag2)
table(df1$A5B5_cat,df1$Y_GC_20221231_lag2)
### Person years
aggregate(df1$Surv_case_GC_20221231/365.25,by=list(type=df1$A5B5C),FUN=sum,na.rm=T)
aggregate(df1$Surv_case_GC_20221231/365.25,by=list(type=df1$A5B5_cat),sum,na.rm=T)

### fit cox model
m3<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231_lag2)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=df1)
m4<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231_lag2)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=df1)
summary(m3)
summary(m4)

## mortality
df1<-dat%>% filter(Y_death_lag2!= 1)
### No. of participants & cases

table(df1$A5B5_cat)
table(df1$A5B5C)
table(df1$A5B5C,df1$Y_GC_death_20221231_lag2)
table(df1$A5B5_cat,df1$Y_GC_death_20221231_lag2)
### Person years
aggregate(df1$Surv_OS_total_20221231/365.25,by=list(type=A5B5C),sum,na.rm=T)
aggregate(df1$Surv_OS_total_20221231/365.25,by=list(type=A5B5_cat),sum,na.rm=T)

### fit cox model
m1<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231_lag2)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=df1)
m2<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231_lag2)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=df1)
summary(m1)
summary(m2)




