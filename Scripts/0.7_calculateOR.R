#########################################################
######    Calculates CI Given Logit Estimates     #######
#########################################################

require(tidyverse)
library(dplyr)
library(tidyr)

calculate_OR <- function(tidy_df) {
  
  # Extract main cluster effects (clusters 1 to 4)
  main_effects <- tidy_df %>%
    filter(grepl("^cluster[1-4]$", term)) %>%
    select(term, estimate, std.error) %>%
    rename(cluster = term,
           main_estimate = estimate,
           main_se = std.error)
  
  # Check if interaction terms with welfare_regime exist
  has_interactions <- any(grepl("^cluster[1-4]:welfare_regime", tidy_df$term))
  
  if (has_interactions) {
    # Extract interaction effects only
    interaction_effects <- tidy_df %>%
      filter(grepl("^cluster[1-4]:welfare_regime", term)) %>%
      separate(term, into = c("cluster", "regime"), sep = ":welfare_regime") %>%
      rename(inter_estimate = estimate,
             inter_se = std.error)
    
    # Baseline regime (reference)
    baseline_df <- main_effects %>%
      mutate(regime = "Mediterranean",
             estimate = main_estimate,
             se = main_se) %>%
      select(cluster, regime, estimate, se)
    
    # Sum main + interaction effects, combine SE (approximate)
    interaction_df <- main_effects %>%
      inner_join(interaction_effects, by = "cluster") %>%
      mutate(estimate = main_estimate + inter_estimate,
             se = sqrt(main_se^2 + inter_se^2)) %>%
      select(cluster, regime, estimate, se)
    
    # Combine baseline and interaction estimates
    final_df <- bind_rows(baseline_df, interaction_df)
    
  } else {
    # No interaction terms found: only main effects, regime = "pooled"
    final_df <- main_effects %>%
      mutate(regime = "pooled",
             estimate = main_estimate,
             se = main_se) %>%
      select(cluster, regime, estimate, se)
  }
  
  # Compute OR and 95% CI, convert cluster to integer
  final_df <- final_df %>%
    mutate(OR = exp(estimate),
           lower_CI = exp(estimate - 1.96 * se),
           upper_CI = exp(estimate + 1.96 * se),
           cluster = as.integer(gsub("cluster", "", cluster))) %>%
    arrange(cluster, regime)
  
  return(final_df)
}

