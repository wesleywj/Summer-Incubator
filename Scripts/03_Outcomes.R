###############################################################################
#Purpose of this script: 
#Produce the later life outcomes, the time-invariant variables and, overall,  
#the information not found in the SHARELIFE survey
#Variables to produce: 
#Instrumental Activities of Daily Living for ages 55-65 and 65-75 
#Activities of Daily Living for ages 55-65 and 65-75 
#Depression (EURO-D scale) for ages 55-65 and 65-75 
#Functional mobility for ages 55-65 and 65-75 
#Years of education
#Migrant status
#Gender
#Country code
###############################################################################

###############################################################################
#### 1 Load librarues and data ####
###############################################################################
library(tidyverse)
library(haven)
library(purrr)

rm(list = ls ())

setwd("N:/Incubator2025_ComputationalLifeCourse/Data")

data <- read_dta("Raw/harmonized_waves.dta") #Load the data


################################################################################
#### 2 Create the function ####
# This function:
# - Takes in a dataframe (`df`), list of waves (`waves`), and a variable stub
# - For each respondent, collects all non-missing values of the variable
#   between ages 55–65 and 65–75 across all waves
# - Averages them if there's more than one value, or keeps a single value if only one
# - Returns the modified dataframe with two new columns
################################################################################
produce_period_averages <- function(df, waves, item_stub, out_prefix,
                                   lower1 = 55, upper1 = 65,
                                   lower2 = 65, upper2 = 75) {
  
#Prepare two list-columns to collect per-person values
  df[[paste0(out_prefix, "_55_65_vals")]] <- vector("list", nrow(df))
  df[[paste0(out_prefix, "_65_75_vals")]] <- vector("list", nrow(df))
  
#Loop over all waves to collect age-appropriate values
  for (w in waves) {
    age_var <- paste0("r", w, "agey")
    itm_var <- paste0("r", w, item_stub)
    
# Skip waves whose variables are missing in the file
    if (!all(c(age_var, itm_var) %in% names(df))) next
    
    age <- df[[age_var]]
    val <- df[[itm_var]]
    
#Logical vectors: identify which rows qualify for each age band and are not NA
    v55_65 <- !is.na(age) & age >= lower1 & age <= upper1 & !is.na(val)
    v65_75 <- !is.na(age) & age >= lower2 & age <= upper2 & !is.na(val)
    
# Append valid values to the corresponding list-column
    df[[paste0(out_prefix, "_55_65_vals")]][v55_65] <-
      Map(c, df[[paste0(out_prefix, "_55_65_vals")]][v55_65], val[v55_65])
    
    df[[paste0(out_prefix, "_65_75_vals")]][v65_75] <-
      Map(c, df[[paste0(out_prefix, "_65_75_vals")]][v65_75], val[v65_75])
  }
  
#Compute the average for each respondent, or NA if no valid values were found
  df[[paste0(out_prefix, "_55_65")]] <- vapply(
    df[[paste0(out_prefix, "_55_65_vals")]],
    function(x) if (length(x)) mean(x) else NA_real_, numeric(1)
  )
  df[[paste0(out_prefix, "_65_75")]] <- vapply(
    df[[paste0(out_prefix, "_65_75_vals")]],
    function(x) if (length(x)) mean(x) else NA_real_, numeric(1)
  )
  
  ## Drop the intermediate list-columns and return
  df %>%
    select(-ends_with("_vals"))
}

###############################################################################
#### 2.1 Create cognitive health score (SHARECog) based on O'Donovan (2023) ####
# Using immediate recall, delayed recall, and verbal fluency scores
# Formula: cogn = imrc * 1.0 + dlrc * 1.5 + verbf * 0.8 
###############################################################################
waves <- c(1, 2, 4, 5, 6, 7, 8, 9)

# Compute the SHARE-Cog score per wave
compute_share_cog <- function(df, wave) {
  imrc_var <- paste0("r", wave, "imrc")
  dlrc_var <- paste0("r", wave, "dlrc")
  verbf_var <- paste0("r", wave, "verbf")
  score_var <- paste0("r", wave, "cogn")  
  
  if (all(c(imrc_var, dlrc_var, verbf_var) %in% names(df))) {
    df[[score_var]] <- df[[imrc_var]] * 1.0 +
      df[[dlrc_var]] * 1.5 +
      df[[verbf_var]] * 0.8
  } else {
    df[[score_var]] <- NA_real_
  }
  return(df)
}


for (w in waves) {
  data <- compute_share_cog(data, w)
}



###############################################################################
#### 3 Apply the function ####
###############################################################################

data <- data %>%
  produce_period_averages(waves, "mobilb",    "mobility")   %>%   #Functional mobility
  produce_period_averages(waves, "adltot6",   "adl")        %>%   #ADL (daily activity limitations)
  produce_period_averages(waves, "iadltot1_s","iadl")       %>%   #IADL (instrumental daily activity limitations)
  produce_period_averages(waves, "eurod",     "depress")    %>%   #Depression (EURO-D scale)
  produce_period_averages(waves, "shlt",      "self_rated") %>%   #Self-rated health (Excellent (1) to Poor (5))
  produce_period_averages(waves, "cogn",         "cogn")          #Cognitive Health (derived)
  

###############################################################################
#### 4 Selection into the trajectories ####
# - Recover information on maternal and paternal educational level
###############################################################################
# Convert parental education variables, keep labels, and drop missing
data <- data %>%
  mutate(
    mom_edu = labelled::to_factor(ramomeducel),
    dad_edu = labelled::to_factor(radadeducel)
  ) 

###############################################################################
#### 5 Health events during treatment ####
# - Produce code to generate yearly observations of these health conditions
# - Produce variables n_years_CONDITION, showing how many years from the 18-50
#   period where spent witht the condition
# - Lastly, produce dichotomous controls for people with conditions before the
#   treatment ch_CONDITION
###############################################################################
# First select conditions
condition_map <- c(
  radiagdiab   = "diabetes",
  radiagcancr  = "cancer",
  radiaglung   = "lung_dis",
  radiagheart  = "heart_prob",
  radiagstrok  = "stroke",
  radiagarthr  = "arthritis",
  radiagparkin = "parkinson",
  radiagpsych  = "psych_cond",
  radiagalzdem = "alzheimer"
)

# Secondly, generate binary age-by-condition indicators from 18 to 50
for (var in names(condition_map)) {
  condition <- condition_map[[var]]
  diag_age <- data[[var]]
  
  for (age in 18:50) {
    new_var <- paste0(condition, "_", age)
    
    data[[new_var]] <- case_when(
      is_tagged_na(diag_age) & na_tag(diag_age) %in% c("s", "x") ~ 0,
      !is.na(diag_age) & diag_age <= age ~ 1,
      !is.na(diag_age) & diag_age > age ~ 0,
      TRUE ~ NA_real_
    )
  }
}

# Thirdy, compute how many years (age 18–50) the condition was present
for (condition in condition_map) {
  year_vars <- paste0(condition, "_", 18:50)
  year_vars_exist <- year_vars[year_vars %in% names(data)]
  condition_matrix <- data[, year_vars_exist]

  # Compute the sum of 1s (years with condition), but return NA if all values are NA
  data[[paste0("n_years_", condition)]] <- apply(condition_matrix, 1, function(row) {
    if (all(is.na(row))) {
      return(NA)
    } else {
      return(sum(row, na.rm = TRUE))
    }
  })
}

# Lastly, if they present the conditions before age 18 generate ch_CONDITION
for (var in names(condition_map)) {
  condition <- condition_map[[var]]
  diag_age <- data[[var]]
  
  data[[paste0("ch_", condition)]] <- case_when(
    !is.na(diag_age) & diag_age < 18 ~ 1,
    !is.na(diag_age) ~ 0,
    is_tagged_na(diag_age) & na_tag(diag_age) %in% c("s", "x") ~ 0,
    TRUE ~ NA_real_
  )
}



###############################################################################
#### 6 Working dataset ####
# - Keep only relevant outcome and background variables
# - Rename some variables for clarity
###############################################################################
data <- data %>%
  select(
    mergeid, 
    mobility_55_65, mobility_65_75,
    adl_55_65, adl_65_75,
    iadl_55_65, iadl_65_75,
    depress_55_65, depress_65_75,
    self_rated_55_65, self_rated_65_75,
    cogn_55_65, cogn_65_75,
    edu_years = raedyrs,
    migrant   = rabcountry,
    sex       = ragender,
    mom_edu, dad_edu,
    country,
    
    # All condition_age variables 
    matches("^(diabetes|cancer|lung_dis|heart_prob|stroke|arthritis|parkinson|psych_cond|alzheimer)_\\d{2}$"),
    
    # Add n_years_condition variables
    matches("^n_years_(diabetes|cancer|lung_dis|heart_prob|stroke|arthritis|parkinson|psych_cond|alzheimer)$"),
    
    # Add ch_condition childhood onset variables
    matches("^ch_(cancer|lung_dis|stroke|arthritis|parkinson|alzheimer)$")
  )

# Keep only respondents with at least one non-missing outcome
data <- data %>%
  filter(
    if_any(
      c("mobility_55_65", "mobility_65_75",
        "adl_55_65", "adl_65_75",
        "iadl_55_65", "iadl_65_75",
        "depress_55_65", "depress_65_75", 
        "cogn_55_65", "cogn_65_75", 
        "depress_55_65", "depress_65_75"),
      ~ !is.na(.x)
    )
  )

# Save final dataset
saveRDS(data, file = "Processed/outcomes.rds")
