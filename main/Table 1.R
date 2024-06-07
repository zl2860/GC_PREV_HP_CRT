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

