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
dat$Age_2cat<-factor(dat$Age_2cat)


df4<-dat%>%filter(Age_2cat==1)
df5<-dat%>%filter(Age_2cat==2)
attach(df4)#<45
attach(df5)#>45
## No. of participants & cases
### incidence
table(A5B5C,Y_GC_20221231)
table(A5B5_cat,Y_GC_20221231)
table(A5B5C,Y_noncardia_GC_20221231)
table(A5B5_cat,Y_noncardia_GC_20221231)
table(A5B5C,Y_cardia_GC_20221231)
table(A5B5_cat,Y_cardia_GC_20221231)
table(A5B5C,Y_unspecified_GC_20221231)
table(A5B5_cat,Y_unspecified_GC_20221231)
table(A5B5C)
table(A5B5_cat)
### mortality
table(A5B5C,Y_GC_death_20221231)
table(A5B5_cat,Y_GC_death_20221231)
table(A5B5C,Y_noncardia_death_20221231)
table(A5B5_cat,Y_noncardia_death_20221231)
table(A5B5C,Y_cardia_death_20221231)
table(A5B5_cat,Y_cardia_death_20221231)
table(A5B5C,Y_unspecified_death)
table(A5B5_cat,Y_unspecified_death)



## fit model
### incidence
m1<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m2<-coxph(Surv(Surv_case_GC_20221231,Y_noncardia_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m3<-coxph(Surv(Surv_case_GC_20221231,Y_cardia_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m4<-coxph(Surv(Surv_case_GC_20221231,Y_unspecified_GC_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)

m5<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m6<-coxph(Surv(Surv_case_GC_20221231,Y_noncardia_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m7<-coxph(Surv(Surv_case_GC_20221231,Y_cardia_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m8<-coxph(Surv(Surv_case_GC_20221231,Y_unspecified_GC_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)

summary(m1)
summary(m5)
summary(m2)
summary(m6)
summary(m3)
summary(m7)
summary(m4)
summary(m8)

### mortality
m1<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m2<-coxph(Surv(Surv_OS_total_20221231,Y_noncardia_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m3<-coxph(Surv(Surv_OS_total_20221231,Y_cardia_death_20221231)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m4<-coxph(Surv(Surv_OS_total_20221231,Y_unspecified_death)~A5B5C+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m5<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m6<-coxph(Surv(Surv_OS_total_20221231,Y_noncardia_death_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m7<-coxph(Surv(Surv_OS_total_20221231,Y_cardia_death_20221231)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)
m8<-coxph(Surv(Surv_OS_total_20221231,Y_unspecified_death)~A5B5_cat+Gender+Age_3cat+Smoking+Alcohol+History_Stomach+History_Cancer)

summary(m1)
summary(m5)
summary(m2)
summary(m6)
summary(m3)
summary(m7)
summary(m4)
summary(m8)
detach()


#Q statistic for heterogeniety

library(meta)
dat_in<-read.csv('E:/mylaptop/7LT/large trial/incidence.csv',header = T)
dat_de<-read.csv('E:/mylaptop/7LT/large trial/death.csv',header = T)
##incidence
###total GC
dff<- dat_in %>% filter(group=='OC')#Overall  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='OA')#Overall  A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='OS')#Overall   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='OF')#Overall   Failed VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='OM')#Overall   Missing VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)
### bon-cardi
dff<- dat_in %>% filter(group=='NC')#non  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='NCA')#non  A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='NS')#   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='NF')#  Failed VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='NM')# Missing VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)
### cardia
dff<- dat_in %>% filter(group=='CC')#  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='CA')# A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='CS')#   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='CF')#  Failed VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)
### unspecified
dff<- dat_in %>% filter(group=='UC')#  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='UA')# A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='US')#   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='UF')#  Failed VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_in %>% filter(group=='UM')# Missing VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

##mortality
###total GC
dff<- dat_de %>% filter(group=='OC')#Overall  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='OA')#Overall  A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='OS')#Overall   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='OF')#Overall   Failed VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='OM')# Missing VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)
### non-cardia cancer
dff<- dat_de %>% filter(group=='NC')#non  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='NCA')#non  A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='NS')#   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='NF')#  Failed VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='NM')# Missing VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)
### cardia 
dff<- dat_de %>% filter(group=='CC')#  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='CA')# A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='CS')#   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='CF')#  fail VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)
### unspecified
dff<- dat_de %>% filter(group=='UC')#  C VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='UA')# A VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='US')#   successful VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)

dff<- dat_de %>% filter(group=='UM')# Missing VS B
metagen(lnHR, lower = lnLOW ,upper = lnUPPER , sm="HR",data=dff)