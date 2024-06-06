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


# Figure
dat$try1<-ifelse(dat$A5B5C=='Group B',2,ifelse(dat$A5B5C=='Group C',3,NA))
dat$try1<-factor(dat$try1,levels = c(3,2),labels = c('Group C','Group B'))
dat$try2<-ifelse(dat$A5B5_cat=='Successful',1,ifelse(dat$A5B5_cat=='Failed',2,NA))
dat$try2<-factor(dat$try2,levels = c(1,2),labels = c('Successful','Failed'))

fit1<-survfit(Surv(Surv_OS_total_20221231/365.25,Y_death_20221231)~try1,data=dat,type="fh")
fit2<-survfit(Surv(Surv_OS_total_20221231/365.25,Y_death_20221231)~try2,data=dat,type="fh")

fit<-list(group1=fit1,group2=fit2)

p1<-ggsurvplot_combine(fit,dat,fun="event",risk.table=TRUE, xlim=c(0,12),xlab="Follow-up(year)",censor.size=3,ylab="Cumulative total mortality rate ",break.x.by=3, palette ="Dark2",ggtheme =  theme_bw(),fontsize=3)

my_vec_graph <- rvg::dml(code = print(p1, newpage = FALSE))

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, my_vec_graph, location = ph_location_fullsize() )
print(doc, target = "Extended_Fig1 .pptx")

# calculate HR

m1<-coxph(Surv(Surv_OS_total_20221231,Y_death_20221231)~relevel(try1,ref='Group B')+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
dat$try2<-ifelse(dat$A5B5_cat=='Successful',1,ifelse(dat$A5B5_cat=='Failed',2,
                                                     ifelse(dat$A5B5_cat=='Group B',0,NA)))

dat$try2<-factor(dat$try2,levels = c(0,1,2),labels = c('Group B','Successful','Failed'))
m2<-coxph(Surv(Surv_OS_total_20221231,Y_death_20221231)~try2+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)

summary(m1)
summary(m2)