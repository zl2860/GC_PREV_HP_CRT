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



# go through the models
## incidecen
attach(dat)
### noncardia
table(dat$A5B5C,dat$Y_noncardia_GC_20221231)
table(dat$A5B5_cat,dat$Y_noncardia_GC_20221231)

m1<-coxph(Surv(Surv_case_GC_20221231,Y_noncardia_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m2<-coxph(Surv(Surv_case_GC_20221231,Y_noncardia_GC_20221231)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)

summary(m1)
summary(m2)

### cardia

table(dat$A5B5C,dat$Y_cardia_GC_20221231)
table(dat$A5B5_cat,dat$Y_cardia_GC_20221231)

m3<-coxph(Surv(Surv_case_GC_20221231,Y_cardia_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m4<-coxph(Surv(Surv_case_GC_20221231,Y_cardia_GC_20221231)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m3)
summary(m4)


### unspecified
table(dat$A5B5C,dat$Y_unspecified_GC_20221231)
table(dat$A5B5_cat,dat$Y_unspecified_GC_20221231)

m5<-coxph(Surv(Surv_case_GC_20221231,Y_unspecified_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m6<-coxph(Surv(Surv_case_GC_20221231,Y_unspecified_GC_20221231)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m5)
summary(m6)
### output 
write_csv(tidy(m1),file = "Extended_table4_incidence.csv" , col_names = TRUE)
write_csv(tidy(m2),file = "Extended_table4_incidence.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m3),file = "Extended_table4_incidence.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m4),file = "Extended_table4_incidence.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m5),file = "Extended_table4_incidence.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m6),file = "Extended_table4_incidence.csv" , col_names = TRUE, append = TRUE)


## mortality
### noncardia
table(dat$A5B5C,dat$Y_noncardia_death_20221231)
table(dat$A5B5_cat,dat$Y_noncardia_death_20221231)

m1<-coxph(Surv(Surv_OS_total_20221231,Y_noncardia_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m2<-coxph(Surv(Surv_OS_total_20221231,Y_noncardia_death_20221231)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m1)
summary(m2)

### cardia
table(dat$A5B5C,dat$Y_cardia_death_20221231)
table(dat$A5B5_cat,dat$Y_cardia_death_20221231)


m3<-coxph(Surv(Surv_OS_total_20221231,Y_cardia_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m4<-coxph(Surv(Surv_OS_total_20221231,Y_cardia_death_20221231)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m3)
summary(m4)


### unspecified
table(dat$A5B5C,dat$Y_unspecified_death)
table(dat$A5B5_cat,dat$Y_unspecified_death)


m5<-coxph(Surv(Surv_OS_total_20221231,Y_unspecified_death)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
m6<-coxph(Surv(Surv_OS_total_20221231,Y_unspecified_death)~factor(A5B5_cat)+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)

### output 
write_csv(tidy(m1),file = "Extended_table4_mortality.csv" , col_names = TRUE)
write_csv(tidy(m2),file = "Extended_table4_mortality.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m3),file = "Extended_table4_mortality.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m4),file = "Extended_table4_mortality.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m5),file = "Extended_table4_mortality.csv" , col_names = TRUE, append = TRUE)
write_csv(tidy(m6),file = "Extended_table4_mortality.csv" , col_names = TRUE, append = TRUE)

detach()