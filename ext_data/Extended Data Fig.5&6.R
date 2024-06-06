#Stratification analysis



#1. Summarize no.of event and Person-years of each group

# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(survival)
library(forestplot)

# Read the dataset
data <- read.csv("MITS.csv")

# Define a function to calculate cases and person-years, handling missing values
calculate_cases_person_years <- function(group_var, value, a5b5_var, a5b5_value) {
  cases <- sum(data[[group_var]] == value & data[[a5b5_var]] == a5b5_value, na.rm = TRUE)
  person_years <- sum(data[data[[group_var]] == value & data[[a5b5_var]] == a5b5_value, "Surv_case_GC_20221231"], na.rm = TRUE) / 365.25
  return(paste(cases, "/", as.integer(person_years)))
}

# Generate the summary table
summary_table <- data.frame(
  group = c("Gender_Male", "Gender_Female", "Smoking_never", "Smoking_ever", "Alcohol_never", "Alcohol_ever", "history_of_GC_2_never", "history_of_GC_2_gastric", "history_of_GC_2_EC", "history_of_GC_2_others", "history.of.stomach.disease_never", "history.of.stomach.disease_ever"),
  symptom_alleviation = c(
    calculate_cases_person_years("Gender", 1, "A5B5", 0),
    calculate_cases_person_years("Gender", 2, "A5B5", 0),
    calculate_cases_person_years("Smoking", 0, "A5B5", 0),
    calculate_cases_person_years("Smoking", 1, "A5B5", 0),
    calculate_cases_person_years("Alcohol", 0, "A5B5", 0),
    calculate_cases_person_years("Alcohol", 1, "A5B5", 0),
    calculate_cases_person_years("history_of_GC_2", 0, "A5B5", 0),
    calculate_cases_person_years("history_of_GC_2", 1, "A5B5", 0),
    calculate_cases_person_years("history_of_GC_2", 2, "A5B5", 0),
    calculate_cases_person_years("history_of_GC_2", 3, "A5B5", 0),
    calculate_cases_person_years("history.of.stomach.disease", 0, "A5B5", 0),
    calculate_cases_person_years("history.of.stomach.disease", 1, "A5B5", 0)
  ),
  successful_eradication = c(
    calculate_cases_person_years("Gender", 1, "A5B5", 1),
    calculate_cases_person_years("Gender", 2, "A5B5", 1),
    calculate_cases_person_years("Smoking", 0, "A5B5", 1),
    calculate_cases_person_years("Smoking", 1, "A5B5", 1),
    calculate_cases_person_years("Alcohol", 0, "A5B5", 1),
    calculate_cases_person_years("Alcohol", 1, "A5B5", 1),
    calculate_cases_person_years("history_of_GC_2", 0, "A5B5", 1),
    calculate_cases_person_years("history_of_GC_2", 1, "A5B5", 1),
    calculate_cases_person_years("history_of_GC_2", 2, "A5B5", 1),
    calculate_cases_person_years("history_of_GC_2", 3, "A5B5", 1),
    calculate_cases_person_years("history.of.stomach.disease", 0, "A5B5", 1),
    calculate_cases_person_years("history.of.stomach.disease", 1, "A5B5", 1)
  ),
  failed_eradication = c(
    calculate_cases_person_years("Gender", 1, "A5B5", 2),
    calculate_cases_person_years("Gender", 2, "A5B5", 2),
    calculate_cases_person_years("Smoking", 0, "A5B5", 2),
    calculate_cases_person_years("Smoking", 1, "A5B5", 2),
    calculate_cases_person_years("Alcohol", 0, "A5B5", 2),
    calculate_cases_person_years("Alcohol", 1, "A5B5", 2),
    calculate_cases_person_years("history_of_GC_2", 0, "A5B5", 2),
    calculate_cases_person_years("history_of_GC_2", 1, "A5B5", 2),
    calculate_cases_person_years("history_of_GC_2", 2, "A5B5", 2),
    calculate_cases_person_years("history_of_GC_2", 3, "A5B5", 2),
    calculate_cases_person_years("history.of.stomach.disease", 0, "A5B5", 2),
    calculate_cases_person_years("history.of.stomach.disease", 1, "A5B5", 2)
  )
)

# Print the result table
print(summary_table)



#2.Calculate HR (95% CI)s


# Function to calculate HR and 95% CI for A5B5=1 vs. 0 or 2 vs. 0 for a specific group
calculate_hr_ci_group <- function(group_var, group_value, comparison) {
  # Subset data for the specific group and comparison
  data_subset <- data[(data$A5B5 == 0 | data$A5B5 == comparison) & data[[group_var]] == group_value,]
  
  # Dynamically create the formula
  formula <- as.formula(paste("Surv(Surv_case_GC_20221231, Y_GC_20221231) ~ factor(A5B5) + Gender + factor(Age_3cat) + Smoking + Alcohol + history_of_GC_2 + history.of.stomach.disease -", group_var))
  
  # Fit Cox proportional hazards model
  model <- coxph(formula, data = data_subset)
  
  # Extract HR and 95% CI for the comparison
  hr <- round(exp(coef(model)[paste("factor(A5B5)", comparison, sep="")]), 2)
  ci <- round(exp(confint(model)[paste("factor(A5B5)", comparison, sep=""),]), 2)
  
  # Create CI string
  ci_str <- paste0(hr, " (", ci[1], "-", ci[2], ")")
  
  return(ci_str)
}

# List of groups and their corresponding values
groups <- list(
  Gender_Male = c("Gender", 1),
  Gender_Female = c("Gender", 2),
  Smoking_never = c("Smoking", 0),
  Smoking_ever = c("Smoking", 1),
  Alcohol_never = c("Alcohol", 0),
  Alcohol_ever = c("Alcohol", 1),
  history_of_GC_2_never = c("history_of_GC_2", 0),
  history_of_GC_2_gastric = c("history_of_GC_2", 1),
  history_of_GC_2_EC = c("history_of_GC_2", 2),
  history_of_GC_2_others = c("history_of_GC_2", 3),
  history_of_stomach_disease_never = c("history.of.stomach.disease", 0),
  history_of_stomach_disease_ever = c("history.of.stomach.disease", 1)
)

# Initialize results dataframe
results_df <- data.frame(
  group = names(groups),
  A5B5_1_vs_0 = NA,
  A5B5_2_vs_0 = NA
)

# Calculate HR and CI for each group
for (i in seq_along(groups)) {
  group_name <- names(groups)[i]
  group_info <- groups[[i]]
  group_var <- group_info[1]
  group_value <- group_info[2]
  
  results_df$A5B5_1_vs_0[i] <- calculate_hr_ci_group(group_var, group_value, 1)
  results_df$A5B5_2_vs_0[i] <- calculate_hr_ci_group(group_var, group_value, 2)
}

# Print the results dataframe
print(results_df)




#3. Forestplot


# Function to calculate HR and 95% CI for A5B5=1 vs. 0 or 2 vs. 0 for a specific group
calculate_hr_ci_group <- function(group_var, group_value, comparison) {
    # Subset data for the specific group and comparison
    data_subset <- data[(data$A5B5 == 0 | data$A5B5 == comparison) & data[[group_var]] == group_value,]
    
    # Dynamically create the formula
    formula <- as.formula(paste("Surv(Surv_case_GC_20221231, Y_GC_20221231) ~ factor(A5B5) + Gender + factor(Age_3cat) + Smoking + Alcohol + history_of_GC_2 + history.of.stomach.disease -", group_var))
    
    # Fit Cox proportional hazards model
    model <- coxph(formula, data = data_subset)
    
    # Extract HR and 95% CI for the comparison
    hr <- round(exp(coef(model)[paste("factor(A5B5)", comparison, sep="")]), 2)
    ci <- round(exp(confint(model)[paste("factor(A5B5)", comparison, sep=""),]), 2)
    
    # Create CI string
    ci_str <- paste0(hr, " (", ci[1], "-", ci[2], ")")
    
    return(list(hr=hr, ci=ci, ci_str=ci_str))
}

# List of groups and their corresponding values
groups <- list(
    Gender_Male = c("Gender", 1),
    Gender_Female = c("Gender", 2),
    Smoking_never = c("Smoking", 0),
    Smoking_ever = c("Smoking", 1),
    Alcohol_never = c("Alcohol", 0),
    Alcohol_ever = c("Alcohol", 1),
    history_of_GC_2_never = c("history_of_GC_2", 0),
    history_of_GC_2_gastric = c("history_of_GC_2", 1),
    history_of_GC_2_EC = c("history_of_GC_2", 2),
    history_of_GC_2_others = c("history_of_GC_2", 3),
    history_of_stomach_disease_never = c("history.of.stomach.disease", 0),
    history_of_stomach_disease_ever = c("history.of.stomach.disease", 1)
)

# Initialize results dataframes
results_df_1 <- data.frame(
    group = names(groups),
    hr_1 = NA,
    lower_1 = NA,
    upper_1 = NA
)

results_df_2 <- data.frame(
    group = names(groups),
    hr_2 = NA,
    lower_2 = NA,
    upper_2 = NA
)

# Calculate HR and CI for each group
for (i in seq_along(groups)) {
    group_name <- names(groups)[i]
    group_info <- groups[[i]]
    group_var <- group_info[1]
    group_value <- group_info[2]
    
    result_1 <- calculate_hr_ci_group(group_var, group_value, 1)
    result_2 <- calculate_hr_ci_group(group_var, group_value, 2)
    
    results_df_1$hr_1[i] <- result_1$hr
    results_df_1$lower_1[i] <- result_1$ci[1]
    results_df_1$upper_1[i] <- result_1$ci[2]
    
    results_df_2$hr_2[i] <- result_2$hr
    results_df_2$lower_2[i] <- result_2$ci[1]
    results_df_2$upper_2[i] <- result_2$ci[2]
}

# Create label text for forest plot
labeltext1 <- results_df_1$group
labeltext2 <- results_df_2$group

# Create the first forest plot (A5B5=1 vs. 0)
p1 <- forestplot(
    labeltext=labeltext1,
    lineheight=unit(2, 'mm'),
    colgap=unit(2, 'mm'),
    rowgap=unit(2, 'mm'),
    graphwidth=unit(40, "mm"),
    ci.vertices.height=0.05,
    mean=results_df_1$hr_1,
    lower=results_df_1$lower_1,
    upper=results_df_1$upper_1,
    zero=1,
    lwd.zero=2.45,
    lty.ci=2.45,
    lwd.ci=2.45,
    lwd.xaxis=2.45,
    clip=c(0.2, 4),
    ci.vertices=TRUE,
    lty=1,
    cex=0.4,
    boxsize=0.085,
    xlab="HR (95% CI)",
    xticks=c(0, 0.5, 1, 1.5, 2),
    family="serif",
    col=fpColors(box="#1c61b6", lines="#1c61b6", zero="gray50")
)

# Create the second forest plot (A5B5=2 vs. 0)
p2 <- forestplot(
    labeltext=labeltext2,
    lineheight=unit(2, 'mm'),
    colgap=unit(2, 'mm'),
    rowgap=unit(2, 'mm'),
    graphwidth=unit(40, "mm"),
    ci.vertices.height=0.05,
    mean=results_df_2$hr_2,
    lower=results_df_2$lower_2,
    upper=results_df_2$upper_2,
    zero=1,
    lwd.zero=2.45,
    lty.ci=2.45,
    lwd.ci=2.45,
    lwd.xaxis=2.45,
    clip=c(0.2, 4),
    ci.vertices=TRUE,
    lty=1,
    cex=0.4,
    boxsize=0.085,
    xlab="HR (95% CI)",
    xticks=c(0, 0.5, 1, 1.5, 2),
    family="serif",
    col=fpColors(box="#1c61b6", lines="#1c61b6", zero="gray50")
)




#4. Same procedure for Ext-Fig6, GC mortality analysis