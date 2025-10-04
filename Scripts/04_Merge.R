###############################################################################
#Purpose of the script:
#Merge three processed SHARE datasets with information on: health trajectories, 
#work/partner/children trajectories, and later-life outcomes—into a single 
#analysis file containing only respondents present in *every* source.
#
# Input files  (in ./Processed):
#   1) health_trajectories.rds   – illness spells (ill_25–ill_50) 
#   2) main_trajectories.rds     – work / children / cohab trajectories (25–50)
#   3) outcomes.rds              – mobility, ADL/IADL, depression, background
# Identifier variables: mergeid
# Output files:
#   operational_dataset.rds  – master file for subsequent analyses
###############################################################################

###############################################################################
#### 1 Load libraries and point R to the Processed folder ####
###############################################################################
library(tidyverse)  
library(haven)
library(dplyr)
library(dplyr)
library(readr)

rm(list = ls ())

setwd("N:/Incubator2025_ComputationalLifeCourse/Data")

###############################################################################
#### 2 Read the three processed datasets  #####
###############################################################################
health_data  <- readRDS("Processed/retrospective_health.rds")   # retrospective info, mainly health
work_data    <- readRDS("Processed/main_trajectories.rds")     # work/partner/children
outcome_data <- readRDS("Processed/outcomes.rds")              # later-life outcomes

###############################################################################
#### 3 Merge the three datasets. Keep only overlapping respondents ##########
###############################################################################
merged_dataset <- list(health_data, work_data, outcome_data) %>%
  reduce(inner_join, by = "mergeid")   # successive inner joins on mergeid


duplicates <- health_data[duplicated(health_data$mergeid), ]
table(table(health_data$mergeid))

duplicates <- work_data[duplicated(work_data$mergeid), ]
table(table(work_data$mergeid))

duplicates <- outcome_data[duplicated(outcome_data$mergeid), ]
table(table(outcome_data$mergeid))

duplicates <- merged_dataset[duplicated(merged_dataset$mergeid), ]
table(table(merged_dataset$mergeid))

###############################################################################
#### 4 Save merged dataset for future analyses ####
###############################################################################
saveRDS(merged_dataset, file = "Processed/operational_dataset.rds")

###############################################################################
#### 5 National subsample ####
# Germany = 12 | Italy = 16 | France = 17 | Sweden = 13 | Denmark = 18
###############################################################################
# France
france <- merged_dataset %>% filter(country == 17)
saveRDS(france, file = "Processed/france.rds")

# Germany
germany <- merged_dataset %>% filter(country == 12)
saveRDS(germany, file = "Processed/germany.rds")

# Italy
italy <- merged_dataset %>% filter(country == 16)
saveRDS(italy, file = "Processed/italy.rds")

# Spain
spain <- merged_dataset %>% filter(country == 15)
saveRDS(spain, file = "Processed/spain.rds")

# Sweden
sweden <- merged_dataset %>% filter(country == 13)
saveRDS(sweden, file = "Processed/sweden.rds")

# Denmark
denmark <- merged_dataset %>% filter(country == 18)
saveRDS(denmark, file = "Processed/denmark.rds")

###############################################################################
#### 6 Welfare Regime Subsamples ####
# Scandinavian      = Sweden (13), Denmark (18), Finland (55)
# Mediterranean     = Spain (15), Italy (16), Greece (19), Portugal (33)
# Corporatist       = Austria (11), Germany (12), France (17), Netherlands (14), 
#                     Belgium (23), Switzerland (20), Luxembourg (31)
# Post-Socialist    = Estonia (34), Slovenia (35), Croatia (47), Lithuania (48), Czech Republic (28),
#                     Bulgaria (51), Romania (61), Slovakia (63), Hungary (32), Poland (29)
# Excluded          = Ireland (30), Israel (25) should be liberal, not enough sample,
#                     Cyprus (53) & Malta (59) sometimes mediterranean, sometimes corporatist.
###############################################################################
# Scandinavian
scandinavian <- merged_dataset %>%
  filter(country %in% c(13, 18, 55))  # Sweden, Denmark, Finland
saveRDS(scandinavian, file = "Processed/scandinavian.rds")

# Mediterranean
mediterranean <- merged_dataset %>%
  filter(country %in% c(15, 16, 19, 33))  # Spain, Italy, Greece, Portugal
saveRDS(mediterranean, file = "Processed/mediterranean.rds")

# Corporatist
corporatist <- merged_dataset %>%
  filter(country %in% c(11, 12, 17, 14, 23, 20, 31))  # Austria, Germany, France, Netherlands, Belgium, Switzerland, Luxembourg
saveRDS(corporatist, file = "Processed/corporatist.rds")

# Post-Socialist
post_socialist <- merged_dataset %>%
  filter(country %in% c(34, 35, 47, 48, 28, 51, 61, 63, 32, 29))  # Estonia, Slovenia, Croatia, Lithuania, Czechia, Bulgaria, Romania, Slovakia, Hungary, Poland
saveRDS(post_socialist, file = "Processed/post_socialist.rds")

# Subsample of 20k individuals
set.seed(123)  # For reproducibility purposes, but we can change seeds
random_subsample <- merged_dataset %>%
  slice_sample(n = 20000)

saveRDS(random_subsample, file = "Processed/random_subsample.rds")

################################################################################
##### Working dataset: 
#Composed by women from three welfare states, Mediterranean, Corporatist and 
#scandinavian. To identify each observation with its respective regime we produce 
#the variable "welfare_regime". Lastly we dichotomize our pshysical outcomes.
# 0 = no limitation, 1 = exist a limitation
################################################################################
# Create working_data with regime label, only females (sex == 2), and binary outcome indicators
working_data <- bind_rows(
  mediterranean %>% mutate(welfare_regime = "Mediterranean"),
  corporatist    %>% mutate(welfare_regime = "Corporatist"),
  scandinavian   %>% mutate(welfare_regime = "Scandinavian")
) %>%
  filter(sex == 2) %>%
  mutate(
    mobility_55_65_dic = if_else(!is.na(mobility_55_65) & mobility_55_65 > 0, 1, 0),
    mobility_65_75_dic = if_else(!is.na(mobility_65_75) & mobility_65_75 > 0, 1, 0),
    adl_55_65_dic      = if_else(!is.na(adl_55_65) & adl_55_65 > 0, 1, 0),
    adl_65_75_dic      = if_else(!is.na(adl_65_75) & adl_65_75 > 0, 1, 0),
    iadl_55_65_dic     = if_else(!is.na(iadl_55_65) & iadl_55_65 > 0, 1, 0),
    iadl_65_75_dic     = if_else(!is.na(iadl_65_75) & iadl_65_75 > 0, 1, 0),
    mobility2_55_65_dic = if_else(!is.na(mobility_55_65) & mobility_55_65 > 1, 1, 0),
    mobility2_65_75_dic = if_else(!is.na(mobility_65_75) & mobility_65_75 > 1, 1, 0),
    adl2_55_65_dic      = if_else(!is.na(adl_55_65) & adl_55_65 > 1, 1, 0),
    adl2_65_75_dic      = if_else(!is.na(adl_65_75) & adl_65_75 > 1, 1, 0),
    iadl2_55_65_dic     = if_else(!is.na(iadl_55_65) & iadl_55_65 > 1, 1, 0),
    iadl2_65_75_dic     = if_else(!is.na(iadl_65_75) & iadl_65_75 > 1, 1, 0),
    self_rated_55_65_dic     = if_else(!is.na(self_rated_55_65) & self_rated_55_65 > 3, 1, 0),
    self_rated_65_75_dic     = if_else(!is.na(self_rated_65_75) & self_rated_65_75 > 3, 1, 0),
    self_rated_65_75_dic     = if_else(!is.na(self_rated_65_75) & self_rated_65_75 > 3, 1, 0),
  ) %>%
  select(-matches("^nchildren_(1[4-9]|[2-4][0-9]|50)$"),
         -hs060_1_raw, -hs060_2_raw, -hs060_3_raw,
         -start1, -stop1, -start2, -stop2, -start3, -stop3
  )

###Dichotomize cognitive decline using education-adjusted residuals
edu_model_data <- working_data %>%
  filter(!is.na(cogn_65_75), !is.na(edu_years))

edu_model <- lm(cogn_65_75 ~ edu_years, data = edu_model_data)

edu_model_data <- edu_model_data %>%
  mutate(
    cogn_resid = resid(edu_model),
    cogn_resid_z = scale(cogn_resid)[, 1],  
    cogn_65_75_dic = ifelse(cogn_resid_z < -1, 1, 0),
    cogn2_65_75_dic = ifelse(cogn_resid_z < -1.5, 1, 0)
  )

saveRDS(working_data, file = "Processed/working_data.rds")

# Convert to long format
working_data_long <- working_data %>%
  pivot_longer(
    cols = matches("^(working_status|cohabitation|ill|diabetes|cancer|lung_dis|heart_prob|stroke|arthritis|parkinson|asthma|psych_cond||alzheimer)_\\d+$"),
    names_to = c(".value", "age"),
    names_pattern = "^(.*)_(\\d+)$"
  ) %>%
  mutate(age = as.integer(age))

saveRDS(working_data_long, file = "Processed/working_data_long.rds")
################################################################################
##### Descriptives ################
################################################################################

# List of country datasets and file names
countries <- list(
  france   = france,
  germany  = germany,
  italy    = italy,
  spain    = spain,
  sweden   = sweden,
  denmark  = denmark
)

# Output directory
output_dir <- "Processed"

# Loop through and reshape + save
for (country_name in names(countries)) {
  
  # Get the dataset
  data_wide <- countries[[country_name]]
  
  # Reshape to long format
  data_long <- data_wide %>%
    pivot_longer(
      cols = matches("^(working_status|cohabitation|ill)_\\d+"),
      names_to = c(".value", "age"),
      names_pattern = "^(.*)_(\\d+)$"
    ) %>%
    mutate(age = as.integer(age))
  
  # Save to file
  saveRDS(data_long, file = file.path(output_dir, paste0(country_name, "_long.rds")))
  
  message("✔ Saved long format for: ", country_name)
  assign(paste0(country_name, "_long"), data_long, envir = .GlobalEnv)  
  
}

# Ensure folder exists
output_dir <- "Processed/descriptives"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# List of country datasets in _long format
countries_long <- list(
  france   = france_long,
  germany  = germany_long,
  italy    = italy_long,
  spain    = spain_long,
  sweden   = sweden_long,
  denmark  = denmark_long
)

# Variables to summarize
numeric_vars <- c(
  "mobility_55_65", "mobility_65_75",
  "adl_55_65", "adl_65_75",
  "iadl_55_65", "iadl_65_75",
  "depress_55_65", "depress_65_75",
  "edu_years"
)

binary_var <- "ill"
categorical_vars <- c("working_status", "cohabitation")

# Loop through each country and sex
for (country_name in names(countries_long)) {
  data <- countries_long[[country_name]]
  data$sex <- as.character(haven::as_factor(data$sex))
  
  for (sex_group in unique(data$sex)) {
    subset_data <- data %>% filter(sex == sex_group)
    
    # Create summary table
    output_table <- tibble(Variable = character(), Value = character(), Percent_or_Mean = numeric(), SD = numeric())
    
    # Numeric summaries
    for (var in numeric_vars) {
      mean_val <- mean(subset_data[[var]], na.rm = TRUE)
      sd_val <- sd(subset_data[[var]], na.rm = TRUE)
      output_table <- bind_rows(output_table, tibble(
        Variable = var,
        Value = "Mean",
        Percent_or_Mean = round(mean_val, 2),
        SD = round(sd_val, 2)
      ))
    }
    
    # Binary (ill) as percent
    ill_pct <- mean(as.numeric(as.character(subset_data[[binary_var]])), na.rm = TRUE) * 100
    output_table <- bind_rows(output_table, tibble(
      Variable = binary_var,
      Value = "Percent ill",
      Percent_or_Mean = round(ill_pct, 1),
      SD = NA
    ))
    
    # Categorical summaries
    for (cat_var in categorical_vars) {
      counts <- subset_data %>%
        count(!!sym(cat_var)) %>%
        mutate(percent = 100 * n / sum(n))
      
      for (i in 1:nrow(counts)) {
        output_table <- bind_rows(output_table, tibble(
          Variable = cat_var,
          Value = as.character(counts[[cat_var]][i]),
          Percent_or_Mean = round(counts$percent[i], 1),
          SD = NA
        ))
      }
    }
    
    # Save CSV
    sex <- gsub("^[0-9]+\\.", "", tolower(sex_group))
    output_file <- file.path(output_dir, paste0(country_name, "_", sex, "_descriptives.csv"))
    write_csv(output_table, output_file)
    message("✔ Saved: ", output_file)
  }
}
