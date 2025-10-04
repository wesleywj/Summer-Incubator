create_msa_rds <- function(data) {
  # Helper to install and load packages
  required_packages <- c("TraMineR", "dplyr", "colorspace", "RColorBrewer", "randomcoloR", "cluster", "Polychrome")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Extract data name for filename
  data_name <- deparse(substitute(data))
  
  # Define fixed output directory
  output_dir <- "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Agnes Files"
  
  # Select relevant columns
  exposure_df <- data %>%
    dplyr::select(
      #      starts_with("nchildren_"),
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
                  "#FCA5A5", "#99000D")
  
  seqact_ch <- seqdef(exposure_df, 34:66,
                      labels = labs_ch,
                      states = scode_ch,
                      cpal = palette_ch)
  
  # Substitution costs
  #  cost_nchild <- seqsubm(seqact_nchild, method = "TRATE")
  cost_emp    <- seqsubm(seqact_emp,    method = "TRATE")
  cost_ch     <- seqsubm(seqact_ch,     method = "TRATE")
  
  # Multichannel dissimilarity
  dissim.MSA <- seqdistmc(list(seqact_emp, seqact_ch),
                          method = "OM",
                          sm = list(cost_emp, cost_ch))
  
  # Output dissimilarity scores to global environment
  assign(paste0(data_name, "_dissim"), dissim.MSA, envir = .GlobalEnv)
  
  # Hierarchical clustering
  agnes_result <- agnes(as.dist(dissim.MSA), method = "ward", keep.diss = FALSE)
  
  # Save to specified directory
  output_path <- file.path(output_dir, paste0(data_name, "_agnes.rds"))
  saveRDS(agnes_result, file = output_path)
  
  # Assign output to global environment
  #  assign("seqact_nchild", seqact_nchild, envir = .GlobalEnv)
  assign(paste0(data_name, "_seqact_emp"), seqact_emp, envir = .GlobalEnv)
  assign(paste0(data_name, "_seqact_ch"), seqact_ch, envir = .GlobalEnv)
  
  message("Agnes object saved to: ", output_path)
  
  return(agnes_result)
}


##############################
## Different cost functions ##
##############################

create_msa_om <- function(data) {
  # Helper to install and load packages
  required_packages <- c("TraMineR", "dplyr", "colorspace", "RColorBrewer", "randomcoloR", "cluster", "Polychrome")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  # Extract data name for filename
  data_name <- deparse(substitute(data))
  
  # Define fixed output directory
  output_dir <- "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Agnes Files"
  
  # Select relevant columns
  exposure_df <- data %>%
    dplyr::select(
      #      starts_with("nchildren_"),
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
                  "#FCA5A5", "#99000D")
  
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
                          sm = list(cost_emp, cost_ch))
  
  # Output dissimilarity scores to global environment
  assign(paste0(data_name, "_dissim"), dissim.MSA, envir = .GlobalEnv)
  
  # Hierarchical clustering
  agnes_result <- agnes(as.dist(dissim.MSA), method = "ward", keep.diss = FALSE)
  
  # Save to specified directory
  output_path <- file.path(output_dir, paste0(data_name, "_agnes.rds"))
  saveRDS(agnes_result, file = output_path)
  
  # Assign output to global environment
  assign(paste0(data_name, "_seqact_emp"), seqact_emp, envir = .GlobalEnv)
  assign(paste0(data_name, "_seqact_ch"), seqact_ch, envir = .GlobalEnv)
  
  message("Agnes object saved to: ", output_path)
  
  return(agnes_result)
}

