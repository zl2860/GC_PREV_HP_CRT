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
dat$try1<-ifelse(dat$A5B5C=='H. pylori negative',1,
                 ifelse(dat$A5B5C=='Symptom alleviation treatment',2,
                        ifelse(dat$A5B5C=='Anti-H. pylori treatment',3,NA)))

# Figure
p1<-ggsurvplot(survfit(Surv(Surv_case_GC_20221231/365.25,Y_GC_20221231)~ try1,data=dat),
               fun="event", 
               risk.table=TRUE,
               xlim=c(0,12),xlab="Follow-up (year)",
               censor.size=3,
               ylim=c(0,0.01),ylab="Cumulative GC incidence rate ",
               break.x.by=3, 
               palette ="Dark2",
               ggtheme =  theme_bw(),
               fontsize=3)

my_vec_graph <- rvg::dml(code = print(p1, newpage = FALSE))

doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, my_vec_graph, location = ph_location_fullsize() )
print(doc, target = "Fig2.pptx",width=5)

# HR
dff<-dat%>% filter(A5B5C=='Group B' | A5B5C=='Group A')
m1<-coxph(Surv(Surv_case_GC_20221231,Y_GC_20221231_old)~A5B5C,data=dff)
summary(m1)