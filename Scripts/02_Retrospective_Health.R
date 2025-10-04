###############################################################################
#Purpose of this script: 
#Produce the variables to control for periods of ill health during ages 14 to 
#50 which are not present in the dataset with the rest of retrospective
#information. The variables we will work with are:
#hs054_: Number of ill periods
#hs059_X: Starting year of ill period number X 
#hs061_X: Stop year of ill period number X
#yrbirth: Year of birth
###############################################################################

###############################################################################
#### 1 Load libraries and data ####
#We will need to load both, SHARELIFE wave 3 and 7. Out of them we will need 2
#modules, "hs" which contains health information, and "cv" which is the 
#coverscreen for year of birth.
#"hs" variables follow the same syntax and structure for both datasets, but
#wave 3 variables are preceded by a "sl_" prefix.
###############################################################################
library(dplyr)
library(tidyr)
library(haven)
library(tibble)
library(TraMineR)
library(ggplot2)
library(purrr)

rm(list = ls ())

setwd("N:/Incubator2025_ComputationalLifeCourse/Data")
#"Raw/share3/sharew3_rel9-0-0_cs.dta"
# Load and merge datasets for wave 3 
data1 <- read_dta("Raw/share3/sharew3_rel9-0-0_hs.dta")
data2 <- read_dta("Raw/share3/sharew3_rel9-0-0_cv_r.dta")
data3 <- read_dta("Raw/share3/sharew3_rel9-0-0_cs.dta")
merge1 <- inner_join(data1, data2, by = "mergeid")
merge2 <- inner_join(merge1, data3, by = "mergeid")
merge2 <- merge2 %>%
  rename_with(~ gsub("^sl_", "", .), starts_with("sl_")) %>% #Eliminate the suffix
  rename_with(~ gsub("^cs", "cc", .), starts_with("cs"))

# Load and merge datasets for wave 7 
data4 <- read_dta("Raw/share7/sharew7_rel9-0-0_hs.dta")
data5 <- read_dta("Raw/share7/sharew7_rel9-0-0_cv_r.dta")
data6 <- read_dta("Raw/share7/sharew7_rel9-0-0_cc.dta")
merge3 <- inner_join(data4, data5, by = "mergeid")
merge4 <- inner_join(merge3, data6, by = "mergeid")

# Combine waves
merge2 <- merge2 %>% mutate(wave = 3)
merge4 <- merge4 %>% mutate(wave = 7)
merged <- full_join(merge2, merge4, by = "mergeid", suffix = c(".w3", ".w7"))
common_vars <- intersect(names(merge2), names(merge4))
common_vars <- setdiff(common_vars, c("mergeid", "wave"))
health_data <- merged %>%
  transmute(
    mergeid,
    !!!setNames(
      lapply(common_vars, function(var) coalesce(merged[[paste0(var, ".w3")]], merged[[paste0(var, ".w7")]])),
      common_vars
    )
  )


# Select only what's strictly necessary
health_data <- health_data %>%
  select(mergeid, yrbirth, hs054_, hs003_, hs004_, hs005_, hs006_,
         hs059_1, hs060_1, hs059_2, hs060_2, hs059_3, hs060_3, 
         cc002_, cc003_, cc008_, cc010_, cc010a_, 
         cc007d1, cc007d2, cc007d3, cc007d4, cc007d5, cc007dno,
         hs008d1, hs008d2, hs008d3, hs008d4, hs008d5, hs008d6,
         hs008d7, hs008d8, hs008d9, hs008d10, hs008dno, hs009d1, 
         hs009d2, hs009d3, hs009d4, hs009d5, hs009d6, hs009d7, 
         hs009d8, hs009d9, hs009dno, hs009dot)

rm(data1, data2, data3, data4, merge1, merge2)


###############################################################################
#### 2 Clean raw illness spell start/end variables ####
#Convert negative codes to NA (Missing codes: -1 = don't know, -2 = refused)
#Also preserve raw values for special codes like 9997 ("still ongoing")
#Since everyone in the sample is 50 or older, "still ongoing" is automatically
#capped out at 50.
#Also relevant, hs054_ answers are: 1 period, 2 periods, 3 periods, 4 or more 
#periods, entire life ill health. 
#Since in the case of 4 or more periods they skip asking for the 3 first periods
#I've just included them with "entire life ill health" as ill for the whole
#period.
###############################################################################
health_data <- health_data %>%
  mutate(across(starts_with("hs059_"), ~ na_if(na_if(.x, -1L), -2L)),       # Start year
         across(starts_with("hs060_"), ~ na_if(na_if(.x, -1L), -2L))) %>%   # End year
  mutate(across(starts_with("hs060_"), .names = "{.col}_raw")) %>%         # Keep raw end year
  mutate(across(ends_with("_raw"), ~ ifelse(.x == 9997L, 9997L, .x))) %>%  # Flag ongoing
  mutate(across(starts_with("hs060_") & !ends_with("_raw"),
                ~ ifelse(.x == 9997L, NA, .x)))    # Treat ongoing as NA for now


###############################################################################
#### 3 Calculate start/stop ages for up to 3 illness spells ####
###############################################################################
# Subtract birth year to get age at start/stop
health_data <- health_data %>%
  mutate(
    start1 = hs059_1 - yrbirth,
    stop1  = case_when(is.na(hs059_1) ~ NA_real_,
                       is.na(hs060_1) ~ 50,
                       TRUE           ~ hs060_1 - yrbirth),
    start2 = hs059_2 - yrbirth,
    stop2  = case_when(is.na(hs059_2) ~ NA_real_,
                       is.na(hs060_2) ~ 50,
                       TRUE           ~ hs060_2 - yrbirth),
    start3 = hs059_3 - yrbirth,
    stop3  = case_when(is.na(hs059_3) ~ NA_real_,
                       is.na(hs060_3) ~ 50,
                       TRUE           ~ hs060_3 - yrbirth)
  )

###############################################################################
#### 4 Flag respondents with invalid spell data ####
# Criteria for invalid spells:
# - No spell count info
# - Spell(s) expected (based on count) but missing start/stop year(s)
# - Account for ongoing spells (coded 9997) or for individuals with 3+ spells
###############################################################################
health_data <- health_data %>%
  mutate(
    invalid_spells = case_when(
      hs054_ %in% c(-1, -2, NA) ~ TRUE,
      hs054_ %in% 1:3 & (
        (hs054_ >= 1 & (is.na(start1) | (is.na(stop1) & hs060_1_raw != 9997L))) |
          (hs054_ >= 2 & (is.na(start2) | (is.na(stop2) & hs060_2_raw != 9997L))) |
          (hs054_ >= 3 & (is.na(start3) | (is.na(stop3) & hs060_3_raw != 9997L)))
      ) ~ TRUE,
      TRUE ~ FALSE
    )
  )

###############################################################################
#### 5 Create year-by-year indicators: ill_14 to ill_50 ####
# For each age 14–50, create a binary variable:
# - 1 if the person was ill at that age
# - 0 if not ill
# - NA if data is missing or invalidages <- 14:50
###############################################################################
ages <- 18:50

for (a in ages) {
  health_data[[paste0("ill_", a)]] <-
    case_when(
      health_data$hs054_ %in% c(-2, -1)         ~ NA_integer_,
      health_data$invalid_spells                ~ NA_integer_,
      health_data$hs054_ %in% c(4, 5)           ~ 1L,          #If more than 3 periods of ill health the whole sequence is 1
      (!is.na(health_data$start1) & a >= health_data$start1 & a <= health_data$stop1) |
        (!is.na(health_data$start2) & a >= health_data$start2 & a <= health_data$stop2) |
        (!is.na(health_data$start3) & a >= health_data$start3 & a <= health_data$stop3) ~ 1L,
      TRUE ~ 0L
    )
}


###############################################################################
#### 6 Prepare and visualize illness trajectories (sequence data) ####
###############################################################################

# Extract only the columns ill_18 to ill_50
ill_seq_data <- health_data %>%
  select(starts_with("ill_"))

# Define sequence object
ill_seq <- seqdef(ill_seq_data,
                  states = c("0", "1"),
                  labels = c("Healthy", "Ill"))

# Plot the first 100 sequences (or fewer if needed)
seqIplot(ill_seq, sortv = "from.start", with.legend = TRUE,
         title = "Illness Trajectories from Age 14 to 50",
         xlab = "Age", ylab = "Individuals")

###############################################################################
#### 7 Childhood health conditions####
#Any of these diseases in your childhood?:
  # Infectious disease (e.g. measles), Polio, Asthma, Respiratory problems, Allergies,
  #Severe diarrhoea, Meningitis/encephalitis, Chronic ear problems, Speech impairment, 
  #Difficulty seeing even with eyeglasses,Severe headaches or migraines, Epilepsy, 
  #fits or seizures, Emotional, nervous, or psychiatric problem, Broken bones, fractures 
  #Appendicitis, Childhood diabetes or high blood sugar, Heart trouble, Leukaemia or 
  #lymphoma, Cancer or malignant tumour
###############################################################################

#### Starting with childhood conditions
health_data <- health_data %>%
  rename(
    ch_infectious   = hs008d1,
    ch_polio        = hs008d2,
    ch_asthma       = hs008d3,
    ch_resp_prob    = hs008d4,
    ch_allergies    = hs008d5,
    ch_diarr        = hs008d6,
    ch_meningitis   = hs008d7,
    ch_ear_probs    = hs008d8,
    ch_speech_prob  = hs008d9,
    ch_sight_prob   = hs008d10,
    
    ch_headaches    = hs009d1,
    ch_epilepsy     = hs009d2,
    ch_psychiatric  = hs009d3,
    ch_fractures    = hs009d4,
    ch_appendicitis = hs009d5,
    ch_diabetes     = hs009d6,
    ch_heart_prob   = hs009d7,
    ch_other_health_probs = hs009dot
  ) %>%
  # Recode to binary: 1 = selected, 0 = not selected, NA = refusal/don’t know
  mutate(across(
    c(ch_infectious, ch_polio, ch_asthma, ch_resp_prob, ch_allergies, ch_diarr,
      ch_meningitis, ch_ear_probs, ch_speech_prob, ch_sight_prob,
      ch_headaches, ch_epilepsy, ch_psychiatric, ch_fractures, ch_appendicitis,
      ch_diabetes, ch_heart_prob, ch_other_health_probs),
    ~ case_when(
      .x == 1 ~ 1,
      .x == 0 ~ 0,
      .x %in% c(-1, -2) ~ NA_real_
    )
  )) %>%
  # Create merged cancer/leukemia variable
  mutate(
    ch_cancer_leukemia = case_when(
      hs009d8 == 1 | hs009d9 == 1 ~ 1,
      hs009d8 == 0 & hs009d9 == 0 ~ 0,
      hs009d8 %in% c(-1, -2) | hs009d9 %in% c(-1, -2) ~ NA_real_
    )
  ) %>%
  # Count total number of childhood conditions
  mutate(
    ch_n_child_cond = rowSums(across(
      c(ch_infectious, ch_polio, ch_asthma, ch_resp_prob, ch_allergies, ch_diarr,
        ch_meningitis, ch_ear_probs, ch_speech_prob, ch_sight_prob,
        ch_headaches, ch_epilepsy, ch_psychiatric, ch_fractures, ch_appendicitis,
        ch_diabetes, ch_heart_prob, ch_cancer_leukemia,
        ch_other_health_probs),
      ~ replace_na(., 0)
    ))
  ) %>%
  # Drop original diagnosis variables not needed anymore
  select(-hs008dno, -hs009dno, -hs009d8, -hs009d9)


################################################################################
#### 8 Childhood health events####
 #Self-rated childhood health. 
 #Ever miss school for more thn a month due to health. 
 #Confined to bed for month than a month due to health. 
 #In hospital for more than a month due to health. 
################################################################################

health_data <- health_data %>%
  # Recode self rated so goes from poor to excellent
  mutate(
    selfrated_health = case_when(
      hs003_ == -1 | hs003_ == -2 ~ NA_real_,
      hs003_ == 6 ~ 5,
      TRUE ~ as.numeric(hs003_)
    ),
    selfrated_health = 6 - selfrated_health  # reverse: new 1 = worst, 5 = best
  ) %>%
  # The rest are dichotomous 1 = happened, 0 = didn't happen
  mutate(
    miss_school = case_when(
      hs004_ == 5 ~ 1,
      hs004_ == 1 ~ 0,
      hs004_ %in% c(-1, -2) ~ NA_real_
    ),
    confined = case_when(
      hs005_ == 5 ~ 1,
      hs005_ == 1 ~ 0,
      hs005_ %in% c(-1, -2) ~ NA_real_
    ),
    hospital = case_when(
      hs006_ == 5 ~ 1,
      hs006_ == 1 ~ 0,
      hs006_ %in% c(-1, -2) ~ NA_real_
    )
  )

################################################################################
#### 9 Childhood Accomodation ####
 #Number of rooms in the residence at age 10.
 #Number of people living in the household at age.
 #Number of features: Fixed bath, Cold running water supply, Hot running water  
                     #supply, Inside toilet, Central heating.
 #Number of books age 10. 
################################################################################
health_data <- health_data %>%
  # Produce number of rooms per person
  mutate(
    rooms = if_else(cc002_ >= 0, cc002_, NA_real_),
    people_hh = if_else(cc003_ >= 0, cc003_, NA_real_),
        room_pp = case_when(
      is.na(rooms) | is.na(people_hh) | people_hh == 0 ~ NA_real_,
      TRUE ~ rooms / people_hh
    ),
    
    # N of books
    n_books = case_when(
      cc008_ >= 1 & cc008_ <= 5 ~ cc008_,
      TRUE ~ NA_real_
    ),
    
    # Gen number of accomodation features
    bath_fix = case_when(cc007d1 == 1 ~ 1, cc007d1 == 0 ~ 0, TRUE ~ NA_real_),
    water_cold = case_when(cc007d2 == 1 ~ 1, cc007d2 == 0 ~ 0, TRUE ~ NA_real_),
    water_hot = case_when(cc007d3 == 1 ~ 1, cc007d3 == 0 ~ 0, TRUE ~ NA_real_),
    toilet_in = case_when(cc007d4 == 1 ~ 1, cc007d4 == 0 ~ 0, TRUE ~ NA_real_),
    heating = case_when(cc007d5 == 1 ~ 1, cc007d5 == 0 ~ 0, TRUE ~ NA_real_),
        total_accom = case_when(
      cc007dno == 1 ~ 0,  # if "none of these", force 0
      TRUE ~ rowSums(across(c(bath_fix, water_cold, water_hot, toilet_in, heating)), na.rm = TRUE)
    )
  )

################################################################################
#### 10 School performance ####
 # Relative position to others in Maths at age 10.
 # Relative position to others in Language at age 10. 
################################################################################
health_data <- health_data %>%
  mutate(
    # Maths performance (didn't attend school = NA)
    maths_performance = case_when(
      cc010_ %in% c(-1, -2, 9) ~ NA_real_,
      cc010_ == 1 ~ 5,
      cc010_ == 2 ~ 4,
      cc010_ == 3 ~ 3,
      cc010_ == 4 ~ 2,
      cc010_ == 5 ~ 1
    ),
    
    # Language performance (didn't attend school = NA)
    lang_performance = case_when(
      cc010a_ %in% c(-1, -2, 9) ~ NA_real_,
      cc010a_ == 1 ~ 5,
      cc010a_ == 2 ~ 4,
      cc010a_ == 3 ~ 3,
      cc010a_ == 4 ~ 2,
      cc010a_ == 5 ~ 1
    ),
   # Maths performance (didn't attend school = 1)
   math_perf2  = case_when(
      cc010_ %in% c(-1, -2) ~ NA_real_,
      cc010_ == 1 ~ 5,
      cc010_ == 2 ~ 4,
      cc010_ == 3 ~ 3,
      cc010_ == 4 ~ 2,
      cc010_ %in% c(5, 9) ~ 1
    ),
        
    # Language performance (didn't attend school = 1)
   lang_perf2  = case_when(
       cc010a_ %in% c(-1, -2) ~ NA_real_,
       cc010a_ == 1 ~ 5,
       cc010a_ == 2 ~ 4,
       cc010a_ == 3 ~ 3,
       cc010a_ == 4 ~ 2,
       cc010a_ %in% c(5, 9) ~ 1
     )
  )

################################################################################
##### Save clean dataset #####
################################################################################

health_data <- health_data %>%
  select(
    -hs054_, -hs003_, -hs004_, -hs005_, -hs006_,
    -hs059_1, -hs060_1, -hs059_2, -hs060_2, -hs059_3, -hs060_3, 
    -cc002_, -cc003_, -cc008_, -cc010_, -cc010a_, 
    -cc007d1, -cc007d2, -cc007d3, -cc007d4, -cc007d5, -cc007dno,
  )

saveRDS(health_data, file = "Processed/retrospective_health.rds")
