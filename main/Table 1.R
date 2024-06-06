#loading packages
library(data.table)
library(dplyr)
library(tableone)
library(eoffice)
library(forestplot)
library(epicalc)
library(ggplot2)
library(survminer)
library(readr)    
library(readOffice)
library(officer)
library(readxl)
# get the data
dat<-fread("E:/mylaptop/0myproject/my project/large_trial_analysis.csv",header=T)
dat1<-fread('E:/mylaptop/0myproject/my project/large_trial_analysis_intervention.csv',header = T)

dat$A5B5C<- factor(dat$A5B5C,levels = c(0,1,2),labels = c('Group B','Group A','Group C'))
dat$Age_3cat<-factor(dat$Age_3cat)
dat$A5B5_cat<-factor(dat$A5B5_cat,levels = c(0,1,2,3),labels = c('Group B','Successful','Failed','Missing'))
dat$Gender<-factor(dat$Gender,levels = c(1,2),labels = c('Male','Female'))
dat$Alcohol2<-factor(dat$Alcohol2,levels = c(0,1,2),labels = c('Never','Ever','Missing'))
dat$Smoking2<-factor(dat$Smoking2,levels = c(0,1,2),labels = c('Never','Ever','Missing'))
dat$History_Cancer<-factor(dat$History_Cancer,levels = c(0,1),labels = c('Never','Ever'))
dat$History_Stomach<-factor(dat$History_Stomach,levels = c(0,1),labels = c('Never','Ever'))
dat$History_Stomach2<-factor(dat$History_Stomach2,levels = c(0,1,2),labels = c('Never','Ever','Missing'))
dat$history_of_GC<-factor(dat$history_of_GC,levels = c(0,1,2,3,4,12),labels = c('Never','GC','EGC','Other cancers','Not known','Both'))
dat$history_of_GC_2<-factor(dat$history_of_GC_2,levels = c(0,1,2,3,12),labels = c('Never','GC','EGC','Other cancers+Not known','Both'))

dat2<-merge(dat,dat1, by= c('ID','BarCode'))
dat2$schoolage1<-ifelse(dat2$schoolage==0 | dat2$schoolage== 1,1,
                        ifelse(dat2$schoolage==2,2,ifelse(dat2$schoolage==3,3,99)))
dat2$schoolage1<-factor(dat2$schoolage1,
                        levels = c(1,2,3,99),
                        labels = c('Primary school or above','Middle school','Junior college or above','Missing'))

dat2$worktype<-factor(dat2$worktype,levels = c(0,1,2,3,99),labels = c('Farmer','Local workers','Migrant workers','Other','Missing'))
dat2$howpeople1<-ifelse(dat2$howpeople >=1 & dat2$howpeople <=3,1,
                        ifelse(dat2$howpeople ==4,2,
                               ifelse(dat2$howpeople >=5 & dat4$howpeople <99,3,99)))
dat2$howpeople1<-factor(dat2$howpeople1,levels = c(1,2,3,99),labels = c('<=3','4','>=5','Missing'))


myvars<-c('Age','Gender','Smoking2','Alcohol2','history_of_GC_2','History_Stomach2','Frequency_missed','schoolage1','worktype','howmoney','howpeople1')
Catvars<-c('Gender','Smoking2','Alcohol2','history_of_GC_2','History_Stomach2','Frequency_missed','schoolage1','worktype','howpeople1')

#Creat table one
tab1<-CreateTableOne(vars = myvars,
                         strata = 'A5B5C',
                         data=dat2,
                         factorVars = Catvars,
                         addOverall=T)
tabexp<-print(tab1,showAllLevels = T)
write.csv(tabexp,file = 'table1-1.csv')

tab1<-CreateTableOne(vars = myvars,
                         strata = 'A5B5_cat',
                         data=dat2,
                         factorVars = Catvars,
                         addOverall=T)
tabexp<-print(tab1,showAllLevels = T)
write.csv(tabexp,file = 'table1-2.csv')

tab1<-CreateTableOne(vars = myvars,
                     strata = 'Miss',
                     data=dat2,
                     factorVars = Catvars,
                     addOverall=T)
tabexp<-print(tab1,showAllLevels = T)
write.csv(tabexp,file = 'table1-3.csv')

