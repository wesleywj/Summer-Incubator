###############################################################################
#Purpose of this script: 
#Produce most of the time-varying variables and create period for ages 18 to 
#50. We will produce:
# working_status: A total of 4 categories. 0 = unemployed, 1 = working, 2 = 
#                 inactive, 3 = in education
# cohabitation = 0 without partner, 1 = living with partner
# nchildren = number of children
###############################################################################

###############################################################################
#### 1. Load libraries and setup ####
###############################################################################
library(dplyr)
library(tidyr)
library(haven)
library(tibble)
library(TraMineR)
library(ggplot2)

rm(list = ls ())

setwd("N:/Incubator2025_ComputationalLifeCourse/Data")

long_data <- read_dta("Raw/work_pannel.dta")

###############################################################################
#### 2 Prepare variables ####
###############################################################################
long_data <- long_data %>%
  mutate(
    working_status = case_when(
      situation %in% c(1, 12) & working_hours %in% c(1, 4) ~ "Full-time employment",
      situation %in% c(1, 12) & working_hours %in% c(2, 3, 5) ~ "Part-time employment",
      situation %in% c(9, 10) ~ "In education",
      situation %in% c(2, 4, 3, 5, 6, 7, 8, 13, 14, 15, 16, 17) ~ "Not working",
      TRUE ~ NA_character_
    ),
    working_status = factor(
      working_status,
      levels = c(
        "Full-time employment",
        "Part-time employment",
        "In education",
        "Not working"
      )
    ),
    cohabitation = case_when(
      withpartner == 1 & nchildren == 0 ~ "Cohabit 0 children",
      withpartner == 0 & nchildren == 0 ~ "Not cohabit 0 children",
      withpartner == 1 & nchildren > 0  ~ "Cohabit with children",
      withpartner == 0 & nchildren > 0  ~ "Not cohabit with children",
      TRUE ~ NA_character_
    ),
    cohabitation = factor(
      cohabitation,
      levels = c(
        "Cohabit 0 children",
        "Not cohabit 0 children",
        "Cohabit with children",
        "Not cohabit with children"
      )
    )
  )
 
###############################################################################
#### 3 Create wide-format dataset for merge with other datasets ####
#Include ranges from 18 to 50 years old exclusively and keep cases with full
#information exclusively
###############################################################################
age_range <- 18:50   # Select range from 18 to 50 years old
trajectories_wide <- long_data %>%
  filter(age %in% age_range) %>%                 # keep only ages 18–50
  select(mergeid, age, nchildren, working_status, cohabitation) %>%
  pivot_wider(
    id_cols    = mergeid,
    names_from = age,
    values_from = c(nchildren, working_status, cohabitation),
    names_glue = "{.value}_{age}"
  ) %>%
  filter(                         # Keep only full-information complete trajectories
    if_all(matches("^(nchildren|working_status|cohabitation)_\\d+$"), ~ !is.na(.x))
  )

###############################################################################
#### 4 Save final wide-format dataset for future merge ####
###############################################################################
saveRDS(trajectories_wide, file = "Processed/main_trajectories.rds")


###############################################################################
#### 5 Graphical output, placeholder, I'll create a new script for it ####
###############################################################################
# Prepare sequence plots (descriptive)
# Long format for sequence analysis
sequences_long <- long_data %>%
  select(mergeid, age, working_status, cohabitation, nchildren)

# Example: Sequence object for working status
working_seq <- sequences_long %>%
  select(mergeid, age, working_status) %>%
  pivot_wider(names_from = age, values_from = working_status) %>%
  column_to_rownames(var = "mergeid")

# Create sequence object
working_seq_obj <- seqdef(working_seq, right = "DEL")

# Plot sequence index plot
png("working_status_trajectories.png", width = 800, height = 600)
seqIplot(working_seq_obj, with.legend = TRUE, main = "Working Status Trajectories")
dev.off()

ggplot(long_data, aes(x = age)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Frequency Distribution of Age",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()

non_missing_summary <- long_data %>%
  group_by(age) %>%
  summarise(
    non_missing_nchildren = sum(!is.na(nchildren)),
    non_missing_cohabitation = sum(!is.na(cohabitation)),
    non_missing_working_status = sum(!is.na(working_status))
  )

non_missing_long <- non_missing_summary %>%
  pivot_longer(
    cols = starts_with("non_missing_"),
    names_to = "variable",
    values_to = "count"
  )

ggplot(non_missing_long, aes(x = age, y = count, color = variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "Number of Non-Missing Observations by Age",
    x = "Age",
    y = "Count",
    color = "Variable"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Dark2")

ggsave("non_missing_observations_by_age.png", width = 8, height = 6)



