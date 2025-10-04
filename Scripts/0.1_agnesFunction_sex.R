create_msa_rds_by_sex <- function(data) {
  # Install and load required packages
  required_packages <- c("TraMineR", "dplyr", "colorspace", "RColorBrewer", "randomcoloR", "cluster", "haven", "Polychrome")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Extract base name for filename
  data_name <- deparse(substitute(data))
  
  # Convert haven_labelled `sex` to character using value labels
  data$sex <- as.character(haven::as_factor(data$sex))
  
  # Define output directory
  output_dir <- "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Agnes Files"
  
  # Only use valid sex categories
  valid_sex_groups <- c("1.man", "2.woman")
  
  for (sex_group in valid_sex_groups) {
    message("Processing: ", sex_group)
    
    # Subset and select relevant columns
    exposure_df <- data %>%
      filter(sex == sex_group) %>%
      dplyr::select(
        starts_with("working_status_"),
        starts_with("cohabitation_")
      )
    
    # Employment sequence
    labs_emp <- c("FT Employment", "Education", "Inactive", "PT Employment", "Unemployed")
    scode_emp <- c("FT", "ED", "IA", "PT", "UE")
    
    # Employment = Greenish; Education = Blue; Inactive = Gray 
    
    palette_emp <- c(
      "#33A02C",  # FT Employment (green)
      "#1F78B4",  # Education (blue)
      "#BDBDBD",  # Inactive (gray)
      "#A6D854",  # PT Employment (light green)
      "#E31A1C"   # Unemployed (red)
    )
    
    seqact_emp <- seqdef(exposure_df, 1:33,
                         labels = labs_emp,
                         states = scode_emp,
                         cpal = palette_emp)
    
################################################################################
##### Cohabitation sequence, had to update it to fit the new categories ####
################################################################################
    # Original code:
    # labs_ch <- c("Not cohabiting", "Cohabiting")
    # scode_ch <- c("NC", "C")
    # palette_ch <- c("#1b9e77", "#d95f02")
    
    ## Doubly new code
    labs_ch <- c("cohab 0 child", "cohab 1 child", "cohab 2 child", "cohab 3+ child",
                 "no cohab 0 child", "no cohab 1 child", "no cohab 2 child", "no cohab 3+ child")
    
    scode_ch <- c("C0", "C1", "C2", "c3",
                  "NC0", "NC1", "NC2", "NC3")
    
    # Red-ish colours represent NC; Blue-ish colours represent C.
    # Darker tones represent more children
    palette_ch <- c(
      "#A6CEE3",  # cohab 0 child (light blue)
      "#1F78B4",  # cohab 1 child (medium blue)
      "#084081",  # cohab 2 child (dark blue)
      "#041C42",  # cohab 3+ child (very dark blue)
      "#FDBF6F",  # no cohab 0 child (light orange)
      "#FB9A29",  # no cohab 1 child (orange)
      "#E31A1C",  # no cohab 2 child (red)
      "#99000D"   # no cohab 3+ child (dark red)
    )
    
    seqact_ch <- seqdef(exposure_df, 34:66,
                        labels = labs_ch,
                        states = scode_ch,
                        cpal = palette_ch)
    
    # Substitution costs
    cost_emp    <- seqsubm(seqact_emp, method = "TRATE")
    cost_ch     <- seqsubm(seqact_ch,  method = "TRATE")
    
    # Multichannel dissimilarity
    dissim.MSA <- seqdistmc(list(seqact_emp, seqact_ch),
                            method = "OM",
                            sm = list(cost_emp, cost_ch),
                            indel = 1,
                            cweight = c(1, 1)
    )
    
    # AGNES clustering
    agnes_result <- agnes(as.dist(dissim.MSA), method = "ward", keep.diss = FALSE)
    
    # Clean label: "1.man" → "man", "2.woman" → "woman"
    sex_label <- gsub("^[0-9]+\\.", "", tolower(sex_group))
    
    # Save result
    output_path <- file.path(output_dir, paste0(data_name, "_", sex_label, "_agnes.rds"))
    saveRDS(agnes_result, file = output_path)
    assign(paste0(data_name, "_dissim_", sex_label), dissim.MSA, envir = .GlobalEnv) #Dissim by sex
    
    # Assign to global environment
    assign(paste0(data_name, "_", "agnes_", sex_label), agnes_result, envir = .GlobalEnv)
    assign(paste0(data_name, "_", "seqact_emp_", sex_label), seqact_emp, envir = .GlobalEnv)
    assign(paste0(data_name, "_", "seqact_ch_", sex_label), seqact_ch, envir = .GlobalEnv)
    
    message("✔ Agnes object saved to: ", output_path)
  }
}


create_msa_om_by_sex <- function(data) {
  # Install and load required packages
  required_packages <- c("TraMineR", "dplyr", "colorspace", "RColorBrewer", "randomcoloR", "cluster", "haven", "Polychrome")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Extract base name for filename
  data_name <- deparse(substitute(data))
  
  # Convert haven_labelled `sex` to character using value labels
  data$sex <- as.character(haven::as_factor(data$sex))
  
  # Define output directory
  output_dir <- "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Agnes Files"
  
  # Only use valid sex categories
  valid_sex_groups <- c("1.man", "2.woman")
  
  for (sex_group in valid_sex_groups) {
    message("Processing: ", sex_group)
    
    # Subset and select relevant columns
    exposure_df <- data %>%
      filter(sex == sex_group) %>%
      dplyr::select(
        starts_with("working_status_"),
        starts_with("cohabitation_")
      )
    
    # Employment sequence
    labs_emp <- c("FT Employment", "Education", "Inactive", "PT Employment", "Unemployed")
    scode_emp <- c("FT", "ED", "IA", "PT", "UE")
    
    # Employment = Greenish; Education = Blue; Inactive = Gray 
    
    palette_emp <- c(
      "#33A02C",  # FT Employment (green)
      "#1F78B4",  # Education (blue)
      "#BDBDBD",  # Inactive (gray)
      "#A6D854",  # PT Employment (light green)
      "#E31A1C"   # Unemployed (red)
    )
    
    seqact_emp <- seqdef(exposure_df, 1:33,
                         labels = labs_emp,
                         states = scode_emp,
                         cpal = palette_emp)
    
    ################################################################################
    ##### Cohabitation sequence, had to update it to fit the new categories ####
    ################################################################################
    # Original code:
    # labs_ch <- c("Not cohabiting", "Cohabiting")
    # scode_ch <- c("NC", "C")
    # palette_ch <- c("#1b9e77", "#d95f02")
    
    ## Doubly new code
    labs_ch <- c("cohab 0 child", "cohab 1 child", "cohab 2 child", "cohab 3+ child",
                 "no cohab 0 child", "no cohab 1 child", "no cohab 2 child", "no cohab 3+ child")
    
    scode_ch <- c("C0", "C1", "C2", "c3",
                  "NC0", "NC1", "NC2", "NC3")
    
    # Red-ish colours represent NC; Blue-ish colours represent C.
    # Darker tones represent more children
    palette_ch <- c(
      "#A6CEE3",  # cohab 0 child (light blue)
      "#1F78B4",  # cohab 1 child (medium blue)
      "#084081",  # cohab 2 child (dark blue)
      "#041C42",  # cohab 3+ child (very dark blue)
      "#FDBF6F",  # no cohab 0 child (light orange)
      "#FB9A29",  # no cohab 1 child (orange)
      "#E31A1C",  # no cohab 2 child (red)
      "#99000D"   # no cohab 3+ child (dark red)
    )
    
    seqact_ch <- seqdef(exposure_df, 34:66,
                        labels = labs_ch,
                        states = scode_ch,
                        cpal = palette_ch)
    
    # Substitution costs
    cost_emp    <- seqsubm(seqact_emp, 
                           method = "CONSTANT",  # Method to determine costs
                           cval = 2,             # Substitution cost
                           with.missing = F,  # Allows for missingness state
                           miss.cost = 1,        # Cost for substituting a missing state
                           time.varying =F, # Does not allow the cost to vary over time
                           weighted = FALSE)
    cost_ch     <- seqsubm(seqact_ch,  
                           method = "CONSTANT",  # Method to determine costs
                           cval = 2,             # Substitution cost
                           with.missing = F,  # Allows for missingness state
                           miss.cost = 1,        # Cost for substituting a missing state
                           time.varying =F, # Does not allow the cost to vary over time
                           weighted = FALSE)
    
    # Multichannel dissimilarity
    dissim.MSA <- seqdistmc(list(seqact_emp, seqact_ch),
                            method = "OM",
                            sm = list(cost_emp, cost_ch),
                            indel = 1,
                            cweight = c(1, 1)
    )
    
    # AGNES clustering
    agnes_result <- agnes(as.dist(dissim.MSA), method = "ward", keep.diss = FALSE)
    
    # Clean label: "1.man" → "man", "2.woman" → "woman"
    sex_label <- gsub("^[0-9]+\\.", "", tolower(sex_group))
    
    # Save result
    output_path <- file.path(output_dir, paste0(data_name, "_", sex_label, "_agnes.rds"))
    saveRDS(agnes_result, file = output_path)
    assign(paste0(data_name, "_dissim_", sex_label), dissim.MSA, envir = .GlobalEnv) #Dissim by sex
    
    # Assign to global environment
    assign(paste0(data_name, "_", "agnes_", sex_label), agnes_result, envir = .GlobalEnv)
    assign(paste0(data_name, "_", "seqact_emp_", sex_label), seqact_emp, envir = .GlobalEnv)
    assign(paste0(data_name, "_", "seqact_ch_", sex_label), seqact_ch, envir = .GlobalEnv)
    
    message("✔ Agnes object saved to: ", output_path)
  }
}








