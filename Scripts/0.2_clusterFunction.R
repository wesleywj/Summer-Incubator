# PLOTTING SEQUENCE TRAJECTORIES
plot_clusters <- function(agnes_object, max_k = 4) {
  # Extract object name as prefix
  prefix <- deparse(substitute(agnes_object))
  country_label <- sub("_agnes", "", prefix)
  
  # Define fixed output directory 
  output_dir <- file.path("N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Sequence Cluster Plots", country_label)
  
  # Get the sequence objects from global environment
  seq_emp    <- get(paste0(country_label, "_seqact_emp"),    envir = .GlobalEnv)
  seq_ch     <- get(paste0(country_label, "_seqact_ch"),     envir = .GlobalEnv)
  
  # Get the country-specific dataframe from global environment
  country_df <- get(country_label, envir = .GlobalEnv)
  country_title_label <- tools::toTitleCase(tolower(country_label))
  
  for (k in 1:max_k) {
    cluster_assignments <- cutree(agnes_object, k) %>% factor()
    
    # Add cluster assignments to the country_df as a new column
    country_df[[paste0("cluster_", k)]] <- cluster_assignments
    assign(country_label, country_df, envir = .GlobalEnv)
    
    # Specify pathname to save cluster plots
    filename <- paste0(country_label, "_dist_k", k, ".png")
    full_path <- file.path(output_dir, filename)
    
    png(full_path, width = 1800, height = 900, res = 150)
    par(mfrow = c(2, k + 1))  # k clusters + 1 legend per row
    
    # employment
    for (i in 1:k) {
      seqdplot(seq_emp[cluster_assignments == i, ],
               xtlab = 18:50, border = NA, with.legend = FALSE,
               main = paste0("Cluster ", i, "- Employment \n(", country_title_label, ")")
      )
    }
    seqlegend(seq_emp, cex = 0.7)
    
    # cohabitation
    for (i in 1:k) {
      seqdplot(seq_ch[cluster_assignments == i, ],
               xtlab = 18:50, border = NA, with.legend = FALSE,
               main = paste0("Cluster ", i, "- Family \n(", country_title_label, ")")
      )
    }
    seqlegend(seq_ch, cex = 0.7)
    
    dev.off()
    message("Saved: ", full_path)
    
    # Sequence Index Plot
    
    ## Specify pathname to save cluster plots
    #(!dir.exists(dirname(full_path))) dir.create(dirname(full_path), recursive = TRUE)
    filename <- paste0(country_label, "_index_k", k, ".png")
    full_path <- file.path(output_dir, filename)
    
    png(full_path, width = 1800, height = 900, res = 150)
    par(mfrow = c(2, k + 1))
    
    # Employment Index Plots
    for (i in 1:k) {
      seqIplot(seq_emp[cluster_assignments == i, ],
               xtlab = 18:50, yaxis = FALSE, with.legend = FALSE,
               main = paste0("Cluster ", i, "- Employment \n(", country_title_label, ")")
      )
    }
    seqlegend(seq_emp, cex = 0.7)
    
    # Cohabitation Index Plots
    for (i in 1:k) {
      seqIplot(seq_ch[cluster_assignments == i, ],
               xtlab = 18:50, yaxis = FALSE, with.legend = FALSE,
               main = paste0("Cluster ", i, "- Family \n(", country_title_label, ")")
      )
    }
    seqlegend(seq_ch, cex = 0.7)
    
    dev.off()
    message("Saved: ", full_path)
  }
}


# PLOTTING SEQUENCE TRAJECTORIES BY SEX (With N in Cluster with Country Label)

plot_clusters_sex <- function(agnes_object, max_k = 4) {
  agnes_name <- deparse(substitute(agnes_object))
  country_label <- sub("_agnes_.*$", "", agnes_name)
  sex_label     <- sub("^.*_agnes_",  "", agnes_name)
  
  seq_emp_nm <- paste0(country_label, "_seqact_emp_", sex_label)
  seq_ch_nm  <- paste0(country_label, "_seqact_ch_",  sex_label)
  
  seq_emp <- get(seq_emp_nm, envir = .GlobalEnv)
  seq_ch  <- get(seq_ch_nm,  envir = .GlobalEnv)
  
  output_dir <- file.path("N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Sequence Cluster Plots", country_label)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  country_title_label <- tools::toTitleCase(tolower(country_label))
  sex_title_label <- tools::toTitleCase(tolower(sex_label))
  
  for (k in 1:max_k) {
    clusters <- cutree(agnes_object, k)
    filename <- paste0(country_label, "_", sex_label, "_dist_k", k, ".png")
    fn <- file.path(output_dir, filename)
    
    png(fn, width = 1800, height = 900, res = 150)
    #par(mfrow = c(2, k + 1), mar = c(2, 2, 2, 1), oma = c(4, 4, 4, 4))
    par(mfrow = c(2, k + 1))
    
    for (i in 1:k) {
      seqdplot(seq_emp[clusters == i, ], border = NA, with.legend = FALSE,
               xtlab = 18:50, 
               main = paste0("Cluster ", i, "- Employment \n(", country_title_label, ", ", sex_title_label, ")")
      )
    }
    seqlegend(seq_emp, cex = 0.7)
    
    for (i in 1:k) {
      seqdplot(seq_ch[clusters == i, ], border = NA, with.legend = FALSE,
               xtlab = 18:50,
               main = paste0("Cluster ", i, "- Family \n(", country_title_label, ", ", sex_title_label, ")")
      )
      
    }
    seqlegend(seq_ch, cex = 0.7)
    
    dev.off()
    message("✔ Saved: ", fn)
  }
  
  # Sequence Index Plot (FIXED: clusters reassigned here)
  for (k in 1:max_k) {
    clusters <- cutree(agnes_object, k)
    
    filename <- paste0(country_label, "_", sex_label, "_index_k", k, ".png")
    full_path <- file.path(output_dir, filename)
    
    png(full_path, width = 1800, height = 900, res = 150)
    par(mfrow = c(2, k + 1))
    
    for (i in 1:k) {
      n_cluster <- sum(clusters == i)
      seqIplot(seq_emp[clusters == i, ], xtlab = 18:50, yaxis = FALSE, with.legend = FALSE,
               main = paste0("Cluster ", i, "\nEmployment \n(", country_title_label, ", ", sex_title_label, ")")
      )
    }
    seqlegend(seq_emp, cex = 0.7)
    
    for (i in 1:k) {
      n_cluster <- sum(clusters == i)
      seqIplot(seq_ch[clusters == i, ], xtlab = 18:50, yaxis = FALSE, with.legend = FALSE,
               main = paste0("Cluster ", i, "\nFamily \n(", country_title_label, ", ", sex_title_label, ")")
      )
    }
    
    seqlegend(seq_ch, cex = 0.7)
    
    dev.off()
    message("✔ Saved: ", full_path)
  }
}


##################################################   
## Partitioning Around Medoids (PAM)            ##
################################################## 

plot_pam <- function(dissim_matrix, k = 4) {
  
  # Get the object name as a string
  dissim_name <- deparse(substitute(dissim_matrix))
  
  # Split into parts
  parts <- strsplit(dissim_name, "_")[[1]]
  
  # Find the position of "dissim"
  dissim_pos <- which(parts == "dissim")
  
  # Country label is everything before "dissim"
  country_label <- paste(parts[1:(dissim_pos - 1)], collapse = "_")
  
  # Sex label: if "dissim" is not the last part, take the next element
  sex_label <- if (length(parts) > dissim_pos) parts[dissim_pos + 1] else NA_character_
  
  # Convert to numeric value for filtering
  sex_val <- switch(sex_label,
                    man = 1,
                    woman = 2,
                    NA)
  
  # Title helper for plot titles
  plot_title_label <- if (!is.na(sex_label)) {
    paste0(country_label, ", ", sex_label)
  } else {
    country_label
  }
  
  # Output directory
  output_dir <- file.path("N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Sequence Cluster Plots", country_label)
  
  # Extract original dataset
  df <- get(country_label, envir = .GlobalEnv)
  
  # Filter by sex
  if (!is.na(sex_val)) {
    df <- df %>% dplyr::filter(sex == sex_val)
  }
  
  # Sequence object names
  if (!is.na(sex_label)) {
    seq_emp_nm <- paste0(country_label, "_seqact_emp_", sex_label)
    seq_ch_nm  <- paste0(country_label, "_seqact_ch_",  sex_label)
  } else {
    seq_emp_nm <- paste0(country_label, "_seqact_emp")
    seq_ch_nm  <- paste0(country_label, "_seqact_ch")
  }
  
  seq_emp <- get(seq_emp_nm, envir = .GlobalEnv)
  seq_ch  <- get(seq_ch_nm,  envir = .GlobalEnv)
  
  # PAM clustering
  pam_result <- pam(dissim_matrix, k, diss = TRUE)
  cluster_labels <- pam_result$clustering
  
  # Assign PAM object to global environment
  pam_output <- paste0("pam_result_", country_label)
  assign(pam_output, pam_result, envir = .GlobalEnv)
  
  ### --- State Distribution Plot ---
  filename <- if (!is.na(sex_label)) {
    paste0(country_label, "_", sex_label, "_dist_k", k, " (PAM).png")
  } else {
    paste0(country_label, "_dist_k", k, " (PAM).png")
  }
  full_path <- file.path(output_dir, filename)
  
  png(full_path, width = 1200, height = 800)
  par(mfrow = c(2, k + 1))
  
  for (i in 1:k) {
    seqdplot(seq_emp[cluster_labels == i, ], border = NA, with.legend = FALSE,
             xtlab = 18:50,
             main = paste0("Cluster ", i, "- Employment\n(", plot_title_label, ")")
    )
  }
  
  seqlegend(seq_emp, cex = 1.5)
  
  for (i in 1:k) {
    seqdplot(seq_ch[cluster_labels == i, ], border = NA, with.legend = FALSE,
             xtlab = 18:50,
             main = paste0("Cluster ", i, "- Family\n(", plot_title_label, ")")
    )
  }
  
  seqlegend(seq_ch, cex = 1.5)
  dev.off()
  cat("\n [OK] PAM Distribution Plot Saved: ", full_path)
  
  ### --- Sequence Index Plot ---
  filename <- if (!is.na(sex_label)) {
    paste0(country_label, "_", sex_label, "_index_k", k, " (PAM).png")
  } else {
    paste0(country_label, "_index_k", k, " (PAM).png")
  }
  full_path <- file.path(output_dir, filename)
  
  png(full_path, width = 1200, height = 800)
  par(mfrow = c(2, k + 1))
  
  for (i in 1:k) {
    seqIplot(seq_emp[cluster_labels == i, ], xtlab = 18:50, yaxis = FALSE, with.legend = FALSE,
             main = paste0("Cluster ", i, "\nEmployment\n(", plot_title_label, ")")
    )
  }
  
  seqlegend(seq_emp, cex = 1.5)
  
  for (i in 1:k) {
    seqIplot(seq_ch[cluster_labels == i, ], xtlab = 18:50, yaxis = FALSE, with.legend = FALSE,
             main = paste0("Cluster ", i, "\nFamily\n(", plot_title_label, ")")
    )
  }
  
  seqlegend(seq_ch, cex = 1.5)
  dev.off()
  cat("\n [OK] PAM Index Plot Saved: ", full_path)
}


##################################################   
#####     Plotting Medoids using PAM            ##
################################################## 


get_medoids <- function(dissim_matrix, k=4) {
  
  # Get the object name as a string
  dissim_name <- deparse(substitute(dissim_matrix))
  
  # Split into parts
  parts <- strsplit(dissim_name, "_")[[1]]
  
  # Find the position of "dissim"
  dissim_pos <- which(parts == "dissim")
  
  # Country label is everything before "dissim"
  country_label <- paste(parts[1:(dissim_pos - 1)], collapse = "_")
  
  # Sex label: if "dissim" is not the last part, take the next element
  sex_label <- if (length(parts) > dissim_pos) parts[dissim_pos + 1] else NA_character_
  
  sex_val <- switch(sex_label,
                    man = 1,
                    woman = 2,
                    NA)
  
  # Define fixed output directory
  output_dir <- file.path("N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Sequence Cluster Plots", country_label)
  
  # Extract country data from global environment
  df <- get(country_label, envir = .GlobalEnv)
  
  # Apply sex filter only if sex_val is 1 or 2
  if (!is.na(sex_val)) {
    df <- df %>% dplyr::filter(sex == sex_val)
  }
  
  # Extract employment and family sequence data
  if (!is.na(sex_label)) {
    seq_emp_nm <- paste0(country_label, "_seqact_emp_", sex_label)
    seq_ch_nm  <- paste0(country_label, "_seqact_ch_",  sex_label)
  } else {
    seq_emp_nm <- paste0(country_label, "_seqact_emp")
    seq_ch_nm  <- paste0(country_label, "_seqact_ch")
  }
  
  seqact_emp <- get(seq_emp_nm, envir = .GlobalEnv)
  seqact_ch  <- get(seq_ch_nm,  envir = .GlobalEnv)
  
  # Obtain PAM results
  pam_label <- paste0("pam_result_", country_label)
  pam_result <- get(pam_label, envir = .GlobalEnv)
  
  # Obtain Cluster Labels
  cluster_labels <- pam_result$clustering
  
  # Obtain medoid indices
  medoid_ids <- pam_result$id.med
  
  # Extract unique ID for medoids
  caseid_medoid <- unique(df$mergeid)[medoid_ids]
  
  # Extract employment/family sequence associated with medoid
  medoid_emp <- suppressMessages(seqformat(seqact_emp[medoid_ids, ], from = "STS", to = "SPS"))
  medoid_ch <- suppressMessages(seqformat(seqact_ch[medoid_ids, ], from = "STS", to = "SPS"))
  
  # Combine each row into one string by collapsing columns with "-"
  medoid_emp_combined <- apply(medoid_emp, 1, function(x) paste(na.omit(x), collapse = "-"))
  medoid_ch_combined <- apply(medoid_ch, 1, function(x) paste(na.omit(x), collapse = "-"))
  
  # Print sequences of medoids
  cat("The caseIDs are:\n")
  print(medoid_ids)
  
  cat("The medoid IDs are \n")
  print(caseid_medoid)
  
  cat("\nMedoid sequences (Employment):\n")
  print(seqact_emp[medoid_ids, ], format = "SPS")
  
  cat("\nMedoid sequences (Family):\n")
  print(seqact_ch[medoid_ids, ], format = "SPS")
  
  cluster_df <- data.frame(
    caseID = caseid_medoid,
    medoidID = medoid_ids,
    emp_seq = medoid_emp_combined,
    fam_seq = medoid_ch_combined
  )
  
  # Plotting Employment Medoid
  ## Filename conditional on whether it's sex-specific or not
  filename <- if (!is.na(sex_label)) {
    paste0(country_label, "_", sex_label, "_medoid_k", k, ".png")
  } else {
    paste0(country_label, "_medoid_k", k, ".png")
  }
  
  full_path <- file.path(output_dir, filename)
  
  # Save plot
  png(full_path, width = 1200, height = 800)
  
  
  par(mfrow = c(2, k+1))  # set layout: 1 row, k+1 columns
  
  for (i in seq_len(k)) {
    seqdplot(seqact_emp[medoid_ids[i], , drop = FALSE],  # single medoid sequence
             xtlab = 18:50, border = NA, with.legend = FALSE,
             main = paste0("Employment Medoid ", i))
  }
  
  seqlegend(seqact_emp, cex = 1.8)
  
  # Plotting Family Medoid
  for (i in seq_len(k)) {
    seqdplot(seqact_ch[medoid_ids[i], , drop = FALSE],  # single medoid sequence
             xtlab = 18:50, border = NA, with.legend = FALSE,
             main = paste0("Family Medoid ", i))
  }
  
  seqlegend(seqact_ch, cex = 1.8)
  dev.off()
  cat("\n [OK] Medoid Plot Saved: ", full_path)
  
  
  # Define separate directory for medoid data
  output_dir2 <- file.path("N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Medoid Data", country_label)
  
  # Construct filename conditionally
  filename2 <- if (!is.na(sex_label)) {
    paste0(country_label, "_medoids_k", k, "_", sex_label, "_df.Rds")
  } else {
    paste0(country_label, "_medoids_k", k, "_df.Rds")
  }
  
  full_path2 <- file.path(output_dir2, filename2)
  
  # Save dataframe of medoid IDs
  saveRDS(cluster_df, file = full_path2)
  
  cat("\n [OK] Medoid IDs Saved: ", full_path2)
  
  return(cluster_df)
}
