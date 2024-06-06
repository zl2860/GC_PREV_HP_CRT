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
dat$incidence_code<-factor(dat$incidence_code,levels = c(21,22,23,26,24,25,33,34,27,30,32,29,35,31,28,42,44,43,38,39,45,46,20,99),
                           labels = c('GC','EC','Lung cancer','Breast cancer','Liver','CRC','Thyroid cancer','Cervical cancer','Brain tumor','Ovarian cancer','Renal cancer','Leukaemia','Pancreatic cancer','Lymphoma','Bladder cancer','Bile duct/gallbladder cancer','Head&neck cancer','Skin cancer','Nasopharyngeal carcinoma','Myelocarcinoma','Prostate cancer','Pituitary tumor','Other cancer','Noncancer')
)


myvars<-c('Y_GC_20221231','Y_noncardia_GC_20221231','Y_cardia_GC_20221231','Y_unspecified_GC_20221231','Y_GC_death_20221231','Y_noncardia_death_20221231','Y_cardia_death_20221231','Y_unspecified_death_20221231','incidence_code')
# creat extended table 2
Extended_Data_Table2<-CreateCatTable(vars = myvars,
                                     strata = 'A5B5C',
                                     data=dat,addOverall=T)
tabexp<-print(Extended_Data_Table2,showAllLevels = T)
write.csv(tabexp,file = 'Extended_Data_Table2-1.csv')

Extended_Data_Table2<-CreateCatTable(vars = myvars,
                                     strata = 'A5B5_cat',
                                     data=dat,addOverall=T)
tabexp<-print(Extended_Data_Table2,showAllLevels = T)
write.csv(tabexp,file = 'Extended_Data_Table2-2.csv')

Extended_Data_Table2<-CreateCatTable(vars = myvars,
                                     strata = 'Miss',
                                     data=dat,addOverall=T)
tabexp<-print(Extended_Data_Table2,showAllLevels = T)
write.csv(tabexp,file = 'Extended_Data_Table2-3.csv')

