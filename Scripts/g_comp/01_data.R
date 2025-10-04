library(tidyverse)
library(dplyr)
library(tibble)
library(writexl)
library(fastDummies)
library(dplyr)
library(stringr)
library(haven)
#################### 
## Read the data  ##
####################

getwd()
setwd("N:/Incubator2025_ComputationalLifeCourse/")

working_data_long = readRDS("Data/Processed/working_data_long.rds")
 

colnames(working_data_long)

############################
## Generate the codebook  ##
############################

codebook <- tibble(
  variable = names(working_data_long),
  type = sapply(working_data_long, function(x) paste(class(x), collapse = ", ")),
  label = sapply(working_data_long, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA else as.character(lbl)
  }),
  n_missing = sapply(working_data_long, function(x) sum(is.na(x))),
  n_unique = sapply(working_data_long, function(x) length(unique(na.omit(x))))
)

codebook <- codebook %>%
  mutate(
    sort_key = case_when(
      variable == "mergeid" ~ 0,
      startsWith(variable, "ch_") ~ 1,
      TRUE ~ 2
    )
  ) %>%
  arrange(sort_key, variable) %>%
  select(-sort_key)  # clean up helper column

writexl::write_xlsx(codebook, "Intermediate/codebook.xlsx")


#######################################
## Rename the variables for analysis ##
#######################################

library(dplyr)
library(stringr)
library(readr)

# Simulated input (replace with your actual data frame)
rename_info <- read_csv("Intermediate/variable_description.csv")   

# Define prefixes by description
prefix_map <- c(
  "ID" = "",
  "pre-treatment vars" = "pre_",
  "treatment vars" = "trt_",
  "during-treatment vars" = "dt_",
  "outcome" = "y_",
  "moderator" = "mod_",
  "time indicator" = "t_"
)

# Apply renaming logic
renaming_plan <- rename_info %>%
  mutate(
    prefix = prefix_map[description],
    new_name = ifelse(
      !is.na(prefix),
      paste0(prefix, variable),
      variable  # leave unchanged if no prefix
    )
  ) %>%
  select(variable, description, new_name)

# View the plan
print(renaming_plan, n = Inf)


# Create lookup vector: names = old variable names, values = new names
rename_vector <- setNames(renaming_plan$new_name, renaming_plan$variable)

# Apply renaming
g_data <- working_data_long %>%
  rename_with(~ rename_vector[.x], .cols = all_of(renaming_plan$variable))


##########################
## Remove factored vars ##
##########################

g_data <- g_data %>%
  mutate(
    # Mother's education
    pre_mom_hs = case_when(
      pre_mom_edu %in% c("4.upper secondary and vocational training", "5.tertiary education") ~ 1,
      pre_mom_edu %in% c("1.less than primary education", "2.primary education", "3.lower secondary education") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Father's education
    pre_dad_hs = case_when(
      pre_dad_edu %in% c("4.upper secondary and vocational training", "5.tertiary education") ~ 1,
      pre_dad_edu %in% c("1.less than primary education", "2.primary education", "3.lower secondary education") ~ 0,
      TRUE ~ NA_real_
    )
  )


g_data <- g_data %>%
  mutate(
    trt_working_status = factor(
      trt_working_status,
      levels = levels(trt_working_status),
      labels = str_replace_all(str_replace_all(tolower(levels(trt_working_status)), " ", "_"), "-", "_")
    ),
    trt_cohabitation = factor(
      trt_cohabitation,
      levels = levels(trt_cohabitation),
      labels = str_replace_all(str_replace_all(tolower(levels(trt_cohabitation)), " ", "_"), "-", "_")
    )
  )

 
g_data <- g_data %>%
  dummy_cols(
    select_columns = c("trt_working_status", "trt_cohabitation"),
    remove_selected_columns = TRUE,
    ignore_na = TRUE
  ) %>%
  rename_with(~ str_replace(., "^trt_working_status_", "trt_"),
              starts_with("trt_working_status_")) %>%
  rename_with(~ str_replace(., "^trt_cohabitation_", "trt_"),
              starts_with("trt_cohabitation_"))

names(g_data)[grep("^trt_", names(g_data))]


##############################
## Countries as dummies ##
##############################


g_data <- g_data %>%
  mutate(mod_country = haven::as_factor(mod_country))

g_data <- g_data %>%
  dummy_cols(
    select_columns = c("mod_country"),
    remove_selected_columns = TRUE,
    ignore_na = TRUE
  ) %>%
  rename_with(tolower, starts_with("mod_country_"))

colnames(g_data)
 
##############################
## Welfare state as dummies ##
##############################

g_data <- g_data %>%
  dummy_cols(
    select_columns = c("mod_welfare_regime"),
    remove_selected_columns = TRUE,
    ignore_na = TRUE
  ) %>%
  rename_with(tolower, starts_with("mod_welfare_regime_"))

 
##########################################
## Generate NAs for binary outcome vars ##
##########################################

# Step 1: Find all variables ending in _dic
dic_vars <- names(g_data) %>% str_subset("_dic$")

# Step 2: Derive base variable names (by stripping _dic)
base_vars <- str_remove(dic_vars, "_dic$")

# Step 3: For each pair, set *_dic := NA where base is NA
for (i in seq_along(dic_vars)) {
  dic_var <- dic_vars[i]
  base_var <- base_vars[i]
  
  if (base_var %in% names(g_data)) {
    g_data[[dic_var]] <- ifelse(is.na(g_data[[base_var]]), NA, g_data[[dic_var]])
  }
}


################################################
##  Select vars that are needed for analysis  ##
################################################
library(haven)
library(dplyr)

clean_haven_labels <- function(df) {
  df %>%
    mutate(across(everything(), zap_labels))  # removes haven_labelled class
}

g_data <- clean_haven_labels(g_data)


g_data <- g_data %>%
  select(
    -pre_dad_edu, 
    -pre_mom_edu,              
    -pre_lang_perf2, 
    -pre_math_perf2,
    -dt_ill,
    -dt_n_years_psych_cond, 
    -`mod_country_israel`,
    -`mod_country_czech republic`,
    -`mod_country_poland`,
    -`mod_country_ireland`,
    -`mod_country_slovenia`,
    -`mod_country_estonia`,
    -`mod_country_croatia`,
    -`mod_country_lithuania`,
    -`mod_country_bulgaria`,
    -`mod_country_cyprus`,
    -`mod_country_latvia`,
    -`mod_country_malta`,
    -`mod_country_romania`,
    -`mod_country_slovakia`,
    -`mod_country_.d:dk`,
    -`mod_country_.m:missing`,
    -`mod_country_.r:refuse`,
    -`mod_country_.u:unmar`,
    -`mod_country_.v:sp nr`
  )

colnames(g_data)

########################################
##  Time-varying co-variate geneation ##
########################################

# Step 1: Identify dt_n_years variables
dt_n_years_vars <- names(g_data)[grepl("^dt_n_years", names(g_data))]

# Step 2: Create binary _dic versions for each
g_data <- g_data %>%
  mutate(across(all_of(dt_n_years_vars),
                ~ ifelse(is.na(.), NA, as.numeric(. > 0)),
                .names = "{.col}_dic"))

# Step 3: Identify new binary _dic variable names
dt_n_years_dic_vars <- paste0(dt_n_years_vars, "_dic")

# Step 4: Create summary binary variable dt_n_years_disease_dic
g_data <- g_data %>%
  rowwise() %>%
  mutate(dt_n_years_disease_dic = {
    vals <- c_across(all_of(dt_n_years_dic_vars))
    if (all(is.na(vals))) NA_real_ else if (any(vals == 1, na.rm = TRUE)) 1 else 0
  }) %>%
  ungroup()

colnames(g_data)

###########################
##  Multiple imputation  ##
###########################

# Load necessary package
library(mice)
# Variables to remove
vars_to_remove <- c(
  # Specific health conditions
  "pre_ch_infectious", "pre_ch_polio", "pre_ch_asthma", "pre_ch_resp_prob", "pre_ch_allergies",
  "pre_ch_diarr", "pre_ch_meningitis", "pre_ch_ear_probs", "pre_ch_speech_prob", "pre_ch_sight_prob",
  "pre_ch_headaches", "pre_ch_epilepsy", "pre_ch_psychiatric", "pre_ch_fractures", "pre_ch_appendicitis",
  "pre_ch_diabetes", "pre_ch_heart_prob", "pre_ch_other_health_probs", "invalid_spells",
  "pre_ch_cancer_leukemia", 
  # Specific residential conditions 
  "pre_bath_fix", "pre_water_cold", "pre_water_hot", "pre_toilet_in", "pre_heating",
  # Specific health conditions
  "dt_n_years_diabetes", "dt_n_years_cancer", "dt_n_years_lung_dis", "dt_n_years_heart_prob",
  "dt_n_years_stroke", "dt_n_years_arthritis", "dt_n_years_parkinson", "dt_n_years_alzheimer",
  "pre_ch_cancer", "pre_ch_lung_dis", "pre_ch_stroke", "pre_ch_arthritis", "pre_ch_parkinson", "pre_ch_alzheimer",
  "dt_diabetes", "dt_cancer", "dt_lung_dis", "dt_heart_prob", "dt_stroke", "dt_arthritis",
  "dt_parkinson", "dt_psych_cond", "dt_alzheimer",
  "dt_n_years_diabetes_dic", "dt_n_years_cancer_dic", "dt_n_years_lung_dis_dic", "dt_n_years_heart_prob_dic",
  "dt_n_years_stroke_dic", "dt_n_years_arthritis_dic", "dt_n_years_parkinson_dic", "dt_n_years_alzheimer_dic",
  # No variation 
  "pre_sex"
)

# Drop the variables from g_data
g_data <- g_data %>% select(-all_of(vars_to_remove)) 

####################################################################
## Check whether time-invariant vars are really not time varying  ##
####################################################################

# Step 1: Select relevant columns
vars_to_check <- g_data %>%
  select(mergeid, starts_with("pre_"))

# Step 2: Pivot to long format
long_check <- vars_to_check %>%
  pivot_longer(cols = -mergeid, names_to = "variable", values_to = "value")

# Step 3: For each individual and variable, count unique values over time
variation_within_id <- long_check %>%
  group_by(mergeid, variable) %>%
  summarise(n_unique = n_distinct(value, na.rm = TRUE), .groups = "drop")

# Step 4: Summarize across individuals: proportion with variation, and if all have no variation
variation_summary <-  variation_within_id %>%
  filter(n_unique > 0) %>%  # Exclude cases where all values are NA
  group_by(variable) %>%
  summarise(
    prop_timevarying = mean(n_unique > 1),
    always_constant = all(n_unique == 1),
    .groups = "drop"
  )


# Step 5: View
print(variation_summary, n = 100)

variation_within_id %>%
  filter(n_unique > 1) %>%
  count(variable, name = "n_people_with_variation") %>%
  arrange(desc(n_people_with_variation))



###############################################################
## Impute only with time-invariant vars: pre_ and dt_n_years ##
###############################################################

colnames(g_data)
# Step 1: Select only pre_ and dt_n_years variables

impute_vars <- g_data %>%
  select(mergeid, starts_with("pre_")) 
 
colnames(impute_vars)
# Collapse to 1 row per person, keeping NAs
collapsed_impute_vars <- impute_vars %>%
  group_by(mergeid) %>%
  summarise(across(everything(), ~ {
    unique_vals <- unique(.)
    unique_vals <- unique_vals[!is.na(unique_vals)]
    if (length(unique_vals) == 1) return(unique_vals)
    else if (length(unique_vals) == 0) return(NA)
    else return(NA)  # conflicting values → treat as missing
  }), .groups = "drop")

# Count proportion of missing per column
collapsed_impute_vars %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(-mergeid, names_to = "variable", values_to = "missing_prop") %>%
  arrange(desc(missing_prop)) %>%
  print(n =100)


# Save mergeid
merge_ids <- collapsed_impute_vars$mergeid

# Drop it before imputation
to_impute <- collapsed_impute_vars %>% select(-mergeid)

# Check missingness pattern (optional)

# Create the plot object
missing_plot <- to_impute %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_prop") %>%
  ggplot(aes(x = reorder(variable, -missing_prop), y = missing_prop)) +
  geom_col(fill = "tomato") +
  labs(title = "Proportion of Missing Values by Variable",
       x = "Variable", y = "Proportion Missing") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot to a file (adjust path/size as needed)
ggsave("Intermediate/missingness_by_variable.png", plot = missing_plot, width = 10, height = 6, dpi = 300)


# Run mice
imputed_data <- mice(to_impute, m = 1, method = "pmm", seed = 2025)
completed_data <- complete(imputed_data, 1)

# Reattach mergeid
completed_data <- bind_cols(mergeid = merge_ids, completed_data)

# Save imputed data 
saveRDS(imputed_data, "Intermediate/imputed_data.rds")
write_rds(completed_data, "Intermediate/imputed_pre_dt_n_years_vars_collapsed.rds")

##########################################
## Replace only those columns in g_data ##
##########################################

# Join imputed wide data back into long format
# Step 1: Drop all 'pre_' and 'dt_n_years' columns
g_data <- g_data %>%
  select(
    -starts_with("pre_")
  )

# Step 2: Join imputed variables (which include pre_ and dt_n_years vars except arthritis_dic)
g_data <- g_data %>%
  left_join(completed_data, by = "mergeid")


# EURO-D Depression Scale (in SHARE)

g_data <- g_data %>% 
  mutate(y_depress_65_75_dic = case_when(
    y_depress_65_75 >= 4 & y_depress_65_75 <= 12 ~ 1,
    y_depress_65_75 >= 0 & y_depress_65_75 < 4 ~ 0,
    TRUE ~ NA_real_  # keeps NAs and handles invalid values like -1
  ))

table(g_data$y_depress_65_75)                                         
 


load("Data/Processed/cluster_data.rds")


g_data <- g_data %>% 
  left_join(cluster_data, by = "mergeid")  



write.csv(g_data, "Data/Processed/g_data.csv", row.names = FALSE)

# Save it as an RDS file
saveRDS(g_data, "Data/Processed/g_data.rds")


g_data %>% 
  filter(mergeid == "AT-010904-02") %>% 
  select(mergeid, t_age, everything()) %>%
  arrange(mergeid, t_age) %>%
  print(n = 100)

#######################################
## Generate the code book for g_data ##
#######################################

g_data_codebook <- tibble(
  variable = names(g_data),
  type = sapply(g_data, function(x) paste(class(x), collapse = ", ")),
  label = sapply(g_data, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA else as.character(lbl)
  }),
  n_missing = sapply(g_data, function(x) sum(is.na(x))),
  n_unique = sapply(g_data, function(x) length(unique(na.omit(x))))
)

g_data_codebook <- g_data_codebook %>%
  mutate(
    sort_key = case_when(
      variable == "mergeid" ~ 0,
      startsWith(variable, "pre_") ~ 1,
      TRUE ~ 2
    )
  ) %>%
  arrange(sort_key, variable) %>%
  select(-sort_key)  # clean up helper column

writexl::write_xlsx(g_data_codebook, "Intermediate/g_data_codebook.xlsx")

#########################
##  Medoid seq data by ##                         
#########################
#"U:/Projects/MPIDR/data/working_data_medoids_k4_df.Rds"
 

# observed medoids 
medoids_k_4df <- readRDS("Analysis/Sequence + Cluster Analysis/Medoid Data/working_data/working_data_medoids_k4_df.Rds")
medoid_mergids <- data.frame(mergeid = medoids_k_4df$caseID)
medoid_mergids %>% left_join(cluster_data, by = "mergeid")
medoid_seq_data <- g_data[g_data$mergeid %in% medoid_mergids$mergeid, ]
medoid_seq_data <- medoid_seq_data %>% mutate(across(where(is.labelled), as_factor))


# no cohabit and no child 
medoid_seq_data_no_cohabit_no_child <- medoid_seq_data %>% 
  mutate(
    trt_cohabit_0_children = 0, 
    trt_not_cohabit_0_children = 1, 
    trt_cohabit_with_children = 0, 
    trt_not_cohabit_with_children = 0
  )

medoid_seq_data_no_cohabit_with_child <- medoid_seq_data %>% 
  mutate(
    trt_cohabit_0_children = 0, 
    trt_not_cohabit_0_children = 0, 
    trt_cohabit_with_children = 0, 
    trt_not_cohabit_with_children = 1
  )

medoid_seq_data_no_cohabit_with_child
 
# Save medoid seq data
write.csv(medoid_seq_data, "Data/Processed/medoid_seq_data.csv", row.names = FALSE)
write.csv(medoid_seq_data_no_cohabit_no_child, "Data/Processed/medoid_seq_data_no_cohabit_no_child.csv", row.names = FALSE)
write.csv(medoid_seq_data_no_cohabit_with_child, "Data/Processed/medoid_seq_data_no_cohabit_with_child.csv", row.names = FALSE)


##################
# clustering id ##
##################
cluster_data <- data.frame(
  mergeid = working_data$mergeid, 
  cluster = pam_result_working_data$clustering
)

# Save as CSV
write.csv(cluster_data, "Data/Processed/cluster_data.csv", row.names = FALSE)
saveRDS(cluster_data, "Data/Processed/cluster_data.rds")

