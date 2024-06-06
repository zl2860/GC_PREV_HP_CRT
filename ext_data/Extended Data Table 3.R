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

## TNM
### T
dat$TNM_T<-ifelse(dat$T =='0',0,
                  ifelse(dat$T == '1' | dat$T == '1a' | dat$T == '1b' | dat$T == 'T1' ,1,ifelse(dat$T == '2' | dat$T == 'T2',2,ifelse(dat$T == 3,3,ifelse(dat$T == '4' | dat$T == '4a' | dat$T == '4b',4,ifelse(dat$T == 'is',5,ifelse(dat$T == 'x',6,99)))))))

dat$TNM_T<-factor(dat$TNM_T,levels = c(0,1,2,3,4,5,6,99),
                  labels = c('T0','T1','T2','T3','T4','Tis','Tx','Unknown'))
### N
dat$TNM_N<-ifelse(dat$N =='0'|dat$N == 'N0',0,
                  ifelse(dat$N == '1' | dat$N == 'N1',1,
                         ifelse(dat$N == '2' ,2,
                                ifelse(dat$N == 3| dat$N == '3a' | dat$N == '3b' |dat$N == 'N3',3,
                                       ifelse(dat$N == 'x',4,99)))))
dat$TNM_N<-factor(dat$TNM_N,levels = c(0,1,2,3,4,99),
                  labels = c('N0','N1','N2','N3','Nx','Unknown'))

### M
dat$TNM_M<-ifelse(dat$M== '0' | dat$M== 'M0',0,
                  ifelse(dat$M=='1',1,
                         ifelse(dat$M== 'x' | dat$M=='X',2,99))
)
dat$TNM_M<-factor(dat$TNM_M,levels = c(0,1,2,99),
                  labels = c('M0','M1','Mx','Unknown'))

dff<-dat%>% filter(dat$Y_GC_20221231==1)


# creat extended table 3 
## Pathology
dff<-dat%>% filter(dat$Y_GC_20221231==1)
sup<-CreateCatTable(vars = 'pathology',
                    strata = 'A5B5C',
                    data=dff,
                    addOverall=T,
)
tabexp<-print(sup,showAllLevels = T)
write.csv(tabexp,file = 'Pathology.csv')




## TNM
dff<-dat%>% filter(dat$Y_GC_20221231==1)
sup<-CreateCatTable(vars = c('TNM_T','TNM_N','TNM_M','T','N','M','A5B5C','Miss'),
                    strata = 'A5B5C',
                    data=dff,
                    addOverall=T,
)
tabexp<-print(sup,showAllLevels = T)
write.csv(tabexp,file = 'TNM-1.csv')
sup<-CreateCatTable(vars = c('TNM_T','TNM_N','TNM_M','T','N','M'),
                    strata = 'Miss',
                    data=dff,
                    addOverall=T,
)
tabexp<-print(sup,showAllLevels = T)
write.csv(tabexp,file = 'TNM-2.csv')

## cTNM & pTNM
### cTNM 
### prepare data
data.cTNM = fread('./data/TNM/large_trial_analysis_20240309.CSV', data.table = F) %>% dplyr::select(A5B5C, Y_GC_20221231, T, N, M) %>% filter(Y_GC_20221231 == 1) %>%
  filter(!is.na(A5B5C)) %>%
  mutate(T = recode(T, 
                    "1" = "T1",
                    "1a" = "T1", 
                    "1b" = "T1",
                    "T1" = "T1",
                    "T2" = "T2",
                    "x" = "Tx",
                    "2" = "T2",
                    "is"="Tis",
                    "3"="T3",
                    "4"="T4",
                    "0"="T0",
                    "4a" = "T4a",
                    "4b" = "T4b"),
         N = recode(N,
                    "1" = "N1",
                    "3a" = "N3",
                    "2" = "N2",
                    "3b" = "N3",
                    "0" = "N0",
                    "N0" = "N0",
                    "3" = "N3",
                    "x" = "Nx"),
         M = recode(M,
                    "1" = "M1",
                    "0" = "M0",
                    "X" = "Mx"))

unique(data.cTNM$T) 
unique(data.cTNM$N) 
unique(data.cTNM$M) 


table(data.TNM$T)

# Function to determine the overall TNM stage based on the criteria provided
determine_cTNM_stage <- function(t, n, m) {
  stages <- matrix(
    c("0", "-", "-", "-", "IVB",
      "I", "IIA", "IIA", "IIA",  "IVB",
      "I", "IIA", "IIA", "IIA",  "IVB",
      "IIB", "III", "III", "III", "IVB",
      "IIB", "III", "III", "III",  "IVB",
      "IVA", "IVA", "IVA", "IVA",  "IVB",
      "IVB", "IVB", "IVB", "IVB", "IVB"),
    nrow = 7, byrow = TRUE,
    dimnames = list(c("Tis", "T1", "T2", "T3", "T4a", "T4b", "Any T, M1"),
                    c("N0", "N1", "N2", "N3", "Any N, M1"))
  )
  
  if (!is.na(m) && m == "M1") {
    return("IVB")
  } else if (is.na(n) || is.na(t)) {
    return(NA)
  } else {
    t_index <- match(t, rownames(stages))
    n_index <- match(n, colnames(stages))
    if (is.na(t_index) || is.na(n_index)) {
      return(NA)
    } else {
      return(stages[t_index, n_index])
    }
  }
}

# Apply the function to the dataframe
# Replace 't_column', 'n_column', 'm_column' with your actual dataframe column names
cTNM_stage <- mapply(determine_cTNM_stage, data.TNM$T, data.TNM$N, data.TNM$M)
data.cTNM = data.cTNM %>% mutate(cTNM_stage = cTNM_stage)
unique(data.TNM$cTNM_stage)

tbl.cTNM = t(table(data.cTNM$A5B5C, data.cTNM$cTNM_stage))
tbl.cTNM
prop.table(tbl.cTNM)*100

rowSums(table(data.cTNM$A5B5C, data.cTNM$cTNM_stage))

table(data.cTNM$A5B5C)


### pTNM
data.pTNM<-fread('./data/TNM/large_trial_analysis_20240309.CSV', data.table = F) %>% dplyr::select(A5B5C, Y_GC_20221231, T, N, M) %>% filter(Y_GC_20221231 == 1) %>%
  filter(!is.na(A5B5C)) %>%
  mutate(T = recode(T, 
                    "1" = "T1",
                    "1a" = "T1", 
                    "1b" = "T1",
                    "T1" = "T1",
                    "T2" = "T2",
                    "x" = "Tx",
                    "2" = "T2",
                    "is"="Tis",
                    "3"="T3",
                    "4"="T4",
                    "0"="T0",
                    "4a" = "T4a",
                    "4b" = "T4b"),
         N = recode(N,
                    "1" = "N1",
                    "3a" = "N3a",
                    "2" = "N2",
                    "3b" = "N3b",
                    "0" = "N0",
                    "N0" = "N0",
                    "3" = "N3",
                    "x" = "Nx"),
         M = recode(M,
                    "1" = "M1",
                    "0" = "M0",
                    "X" = "Mx"))

unique(data.pTNM$T) 
unique(data.pTNM$N) 
unique(data.pTNM$M) 


table(data.pTNM$T)

# Function to determine the overall TNM stage based on the criteria provided
determine_pTNM_stage <- function(t, n, m) {
  stages <- matrix(
    c("0", "-", "-", "-", "-", "IV",
      "IA", "IB", "IIA", "IIB", "IIIB","IV",
      "IB", "IIA", "IIB", "IIIA", "IIIB", "IV",
      "IIA", "IIB", "IIIA", "IIIB", "IIIC" , "IV",
      "IIB", "IIIA", "IIIA", "IIIB", "IIIC",  "IV",
      "IIIA", "IIIB", "IIIB", "IIIC", "IIIC", "IV",
      "IV", "IV", "IV", "IV", "IV", "IV"),
    nrow = 7, byrow = TRUE,
    dimnames = list(c("Tis", "T1", "T2", "T3", "T4a", "T4b", "Any T, M1"),
                    c("N0", "N1", "N2", "N3a", "N3b", "Any N, M1"))
  )
  
  if (!is.na(m) && m == "M1") {
    return("IV")
  } else if (is.na(n) || is.na(t)) {
    return(NA)
  } else {
    t_index <- match(t, rownames(stages))
    n_index <- match(n, colnames(stages))
    if (is.na(t_index) || is.na(n_index)) {
      return(NA)
    } else {
      return(stages[t_index, n_index])
    }
  }
}

# Apply the function to the dataframe
# Replace 't_column', 'n_column', 'm_column' with your actual dataframe column names
pTNM_stage <- mapply(determine_pTNM_stage, data.pTNM$T, data.pTNM$N, data.pTNM$M)
data.pTNM = data.pTNM %>% mutate(pTNM_stage = pTNM_stage)

unique(pTNM_stage)
unique(data.pTNM$pTNM_stage)

tbl.pTNM = t(table(data.pTNM$A5B5C, data.pTNM$pTNM_stage))
tbl.pTNM
prop.table(tbl.pTNM)*100

rowSums(table(data.pTNM$A5B5C, data.pTNM$pTNM_stage))

table(data.pTNM$A5B5C)
