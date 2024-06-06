# Required packages
library(readxl)
library(tidyverse)
library(data.table)
library(cmprsk)
library(lme4)
library(sjstats)

# Load and clean data
data <- read_xlsx('/path/to/data_set_for_competing_risk_analysis.xlsx') %>%
  mutate(Y_death_only = ifelse(Y_death_20221231==1 & Y_GC_20221231 ==0, 1, 0),
         Y_death_with_GC = ifelse(Y_death_20221231==1 & Y_GC_20221231 ==1, 1, 0),
         Y_incidence_compete = case_when(
           Y_death_20221231==1 & Y_GC_20221231 ==0 ~ 2, # only death (competing event)
           Y_death_20221231==1 & Y_GC_20221231 ==1 ~ 1, # having GC
           Y_death_20221231==0 & Y_GC_20221231 ==0 ~ 0, # censoring
           Y_death_20221231==0 & Y_GC_20221231 ==1 ~ 1 # having GC
         ),
         Y_mortality_compete = case_when(
           Y_death_20221231 ==0 & Y_GC_death_20221231 == 0 ~ 0, # censoring
           Y_death_20221231 ==0 & Y_GC_death_20221231 == 1 ~ 1, # die of GC
           Y_death_20221231 == 1 & Y_GC_death_20221231 == 1 ~ 1, # die of GC
           Y_death_20221231 == 1 & Y_GC_death_20221231 == 0 ~ 2 # die from other causes (competing event)
         )
  )

# Define the competing risk analysis function
run_competing_risk_analysis <- function(Surv_time, Status, trt_group, age_sex_only, data, no_adjust=F){
  # Convert string names to symbols
  Surv_time <- rlang::sym(Surv_time)
  Status <- rlang::sym(Status)
  trt_group <- rlang::sym(trt_group)
  
  # Create a data frame subset for analysis
  data.analysis <- data %>%
    filter(!is.na(!!trt_group)) %>%
    mutate(!!trt_group := factor(!!trt_group))
  
  # Create covariate matrix
  cov_mat <- if (no_adjust) {
    data.analysis %>%
      select(!!trt_group)
  } else {
    data.analysis %>%
      select(!!trt_group, "Gender", "Age_3cat", "Smoking", "Alcohol", "History_Cancer", "History_Stomach")
  }
  
  # Fit the competing risk regression model
  fit <- cmprsk::crr(
    ftime = data.analysis[[as.character(Surv_time)]],
    fstatus = data.analysis[[as.character(Status)]],
    cov1 = model.matrix(~ . , data = cov_mat)[, -1],
    tf = function(t) t,
    failcode = 1,
    cencode = 0
  )
  
  print("Analysis done")
  return(fit)
}

# Run analysis for incidence and mortality as per your table
# Replace 'Surv_time', 'Status', 'trt_group', and other parameters as necessary
results.incidence <- run_competing_risk_analysis("Surv_case_GC_20221231", "Y_incidence_compete", "A5B5C", FALSE, data)
results.mortality <- run_competing_risk_analysis("Surv_OS_total_20221231", "Y_mortality_compete", "A5B5C", FALSE, data)

# Print results
print(summary(results.incidence))
print(summary(results.mortality))
