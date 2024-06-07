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
dat_figure4<-fread("E:/mylaptop/7LT/large trial/Figure4.csv",header=T)

# plot
## successful
dff<-dat_figure4 %>% filter( C1 =='successful')
attach(dff)
p1<- forestplot( 
  labeltext=as.matrix(dff[,c(2,3)]),
  graph.pos=2,
  lineheight = unit(7,'mm'),
  colgap = unit(2,'mm'),
  rowgap = unit(10,'mm'),
  graphwidth=unit(30,"mm"),
  ci.vertices.height = 0.05,
  mean=C4,lower=C5,upper=C6,
  zero=1,
  lwd.zero = 2.45,
  lty.ci=2.45,
  lwd.ci = 2.4,
  lwd.xaxis=2.45,
  clip = c(0.1,2.5),
  ci.vertices=T,
  lty=1,
  cex=0.4,
  boxsize=0.1,
  xlab="HR(95%CI)",
  xticks=c(0,0.5,1,1.5,2),
  family="serif",
  col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50")
  
)

topptx(p1,"F4_successful.pptx",height=7,width=6)

## Failed
dff<-dat_figure4 %>% filter(C1=='failed')
attach(dff)
p1<- forestplot( 
  labeltext=as.matrix(dff[,c(2,3)]),
  graph.pos=2,
  lineheight = unit(7,'mm'),
  colgap = unit(2,'mm'),
  rowgap = unit(10,'mm'),
  graphwidth=unit(30,"mm"),
  ci.vertices.height = 0.05,
  mean=C4,lower=C5,upper=C6,
  zero=1,
  lwd.zero = 2.45,
  lty.ci=2.45,
  lwd.ci = 2.4,
  lwd.xaxis=2.45,
  clip = c(0.1,2.5),
  ci.vertices=T,
  lty=1,
  cex=0.4,
  boxsize=0.1,
  xlab="HR(95%CI)",
  xticks=c(0,0.5,1,1.5,2),
  family="serif",
  col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50")
  
)

topptx(p1,"F4_fail.pptx",height=7,width=6)

### H. pylori negative
dff<-dat_figure4 %>% filter(C1=='Group C')
attach(dff)
p1<- forestplot( 
  labeltext=as.matrix(dff[,c(2,3)]),
  graph.pos=2,
  lineheight = unit(7,'mm'),
  colgap = unit(2,'mm'),
  rowgap = unit(10,'mm'),
  graphwidth=unit(30,"mm"),
  ci.vertices.height = 0.05,
  mean=C4,lower=C5,upper=C6,
  zero=1,
  lwd.zero = 2.45,
  lty.ci=2.45,
  lwd.ci = 2.4,
  lwd.xaxis=2.45,
  clip = c(0.1,2.5),
  ci.vertices=T,
  lty=1,
  cex=0.4,
  boxsize=0.1,
  xlab="HR(95%CI)",
  xticks=c(0,0.5,1,1.5,2),
  family="serif",
  col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50")
  
)

topptx(p1,"F4_groupC.pptx",height=7,width=6)
