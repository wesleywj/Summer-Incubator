getSeq <- function(data) {
  # Helper to install and load packages
  required_packages <- c("TraMineR", "dplyr", "colorspace", "RColorBrewer", "randomcoloR", "cluster", "Polychrome")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Extract data name
  data_name <- deparse(substitute(data))
  
  # Split into parts
  parts <- strsplit(data_name, "_")[[1]]
  
  # Determine if sex label is present
  if (tail(parts, 1) %in% c("man", "woman")) {
    sex_label <- tail(parts, 1)
    country_label <- paste(parts[-length(parts)], collapse = "_")
  } else {
    sex_label <- NA_character_
    country_label <- data_name
  }
  
  # Select relevant columns
  exposure_df <- data %>%
    dplyr::select(
      starts_with("working_status_"),
      starts_with("cohabitation_")
    )
  
  # Employment sequence
  labs_emp <- c("FT Employment", "Education", "Out of Labour Force", "PT Employment")
  scode_emp <- c("FT", "ED", "OOLF", "PT")
  palette_emp <- c("#33A02C", "#1F78B4", "#E31A1C", "#A6D854")
  
  seqact_emp <- seqdef(exposure_df, 1:33,
                       labels = labs_emp,
                       states = scode_emp,
                       cpal = palette_emp)
  
  # Cohabitation sequence
  labs_ch <- c("cohab w/o child", "cohab with child", 
               "not cohab w/o child", "not cohab with child")
  scode_ch <- c("C0", "C1+", "NC0", "NC1+")
  palette_ch <- c("#A6CEE3", "#041C42",
                  "#FDBF6F", "#99000D")
  
  seqact_ch <- seqdef(exposure_df, 34:66,
                      labels = labs_ch,
                      states = scode_ch,
                      cpal = palette_ch)
  
  # Substitution costs
  cost_emp <- seqsubm(seqact_emp, method = "TRATE")
  cost_ch  <- seqsubm(seqact_ch,  method = "TRATE")
  
  # Multichannel dissimilarity
  dissim.MSA <- seqdistmc(list(seqact_emp, seqact_ch),
                          method = "OM",
                          sm = list(cost_emp, cost_ch))
  
  # Set appropriate names based on sex label
  if (!is.na(sex_label)) {
    emp_nm <- paste0(country_label, "_seqact_emp_", sex_label)
    ch_nm  <- paste0(country_label, "_seqact_ch_",  sex_label)
    diss_nm <- paste0(country_label, "_dissim_", sex_label)
  } else {
    emp_nm <- paste0(country_label, "_seqact_emp")
    ch_nm  <- paste0(country_label, "_seqact_ch")
    diss_nm <- paste0(country_label, "_dissim")
  }
  
  # Assign to global environment
  assign(emp_nm, seqact_emp, envir = .GlobalEnv)
  assign(ch_nm, seqact_ch, envir = .GlobalEnv)
  assign(diss_nm, dissim.MSA, envir = .GlobalEnv)
}
