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
dat$A6B6_cat<-factor(dat$A6B6_cat,levels = c(0,1,2,3,4,5),
                     labels = c('B-failed','A-successful','A-failed','A-missing','B-successful','B-missing'))

dat$try1<-ifelse(dat$A6B6_cat=='A-failed' | dat$A6B6_cat=='B-failed' | dat$A6B6_cat=='A-missing'| dat$A6B6_cat=='B-missing',1,
                 ifelse(dat$A6B6_cat=='A-successful' |dat$A6B6_cat=='B-successful',2,NA) )
dat$try1<-factor(dat$try1,levels = c(1,2),
                 labels = c('H. pylori positive posttreatment
','H. pylori negative posttreatment'))

# Gastric cancer incidence(left)
## Figure 
p1<-ggsurvplot(survfit(Surv(Surv_case_GC_20221231/365.25,Y_GC_20221231) ~ try1, data=dat),
               fun="event", 
               pval=TRUE,
               ylim=c(0,0.008),
               risk.table = TRUE,
               xlim=c(0,12),xlab="Follow-up(year)",
               censor.size=3,
               ylab="Cumulative GC incidence rate ",
               break.x.by=3, palette ="Dark2",ggtheme =  theme_bw(),fontsize=3)

my_vec_graph <- rvg::dml(code = print(p1, newpage = FALSE))

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, my_vec_graph, location = ph_location_fullsize() )
print(doc, target = "Extended_Fig3_incidence.pptx",width=5)

## HR
m1<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231)~try1+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m1)

# Gastric cancer mortality(Right)
## Figure

p1<-ggsurvplot(survfit(Surv(Surv_OS_total_20221231/365.25,Y_GC_death_20221231) ~ try1, data=dat),
               fun="event", 
               pval=TRUE,
               ylim=c(0,0.004),
               risk.table = TRUE,
               xlim=c(0,12),xlab="Follow-up(year)",
               censor.size=3,
               ylab="Cumulative GC mortality rate ",
               break.x.by=3, palette ="Dark2",ggtheme =  theme_bw(),fontsize=3)

my_vec_graph <- rvg::dml(code = print(p1, newpage = FALSE))

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, my_vec_graph, location = ph_location_fullsize() )
print(doc, target = "Extended_Fig3_death.pptx",width=5)

## HR

m2<-coxph(Surv(Surv_OS_total_20221231,Y_GC_death_20221231)~try1+Gender+Age_3cat+Smoking+Alcohol+History_Cancer+History_Stomach,data=dat)
summary(m2)