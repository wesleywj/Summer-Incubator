require(TraMineR)
require(cluster)

###############################################################
###         Obtain Pairwise Dissimilarity Comparisons       ###
###############################################################

cluster_het <- function(dissim_matrix) {
  # Get name of dissimilarity matrix and country label
  dissim_name <- deparse(substitute(dissim_matrix))
  country_label <- sub("_dissim$", "", dissim_name)
  
  # Dynamically retrieve PAM result object from global environment
  pam_result <- get(paste0("pam_result_", country_label), envir = .GlobalEnv)
  
  # Extract cluster info
  cluster_labels <- pam_result$clustering
  cluster_medoids <- pam_result$medoids
  unique_clusters <- unique(cluster_labels)
  
  # 1. Non-standardised pairwise dissimilarity w/ medoid
  wmd <- sapply(unique_clusters, function(k) {
    medoid_index <- cluster_medoids[k]
    cluster_indices <- which(cluster_labels == k)
    mean(dissim_matrix[cluster_indices, medoid_index])
  })
  
  # 2. Standardised pairwise dissimilarity w/ medoid
  diss_vec <- as.vector(dissim_matrix)
  diss_scaled_vec <- scale(diss_vec)
  diss_scaled <- matrix(diss_scaled_vec,
                        nrow = nrow(dissim_matrix),
                        ncol = ncol(dissim_matrix),
                        byrow = FALSE)
  dimnames(diss_scaled) <- dimnames(dissim_matrix)
  
  wmd_scaled <- sapply(unique_clusters, function(k) {
    medoid_index <- cluster_medoids[k]
    cluster_indices <- which(cluster_labels == k)
    mean(diss_scaled[cluster_indices, medoid_index])
  })
  
  
  # Save results
  out_dir <- "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Cluster Heterogeneity"
  
  saveRDS(wmd,        file = file.path(out_dir, paste0("wmd_", country_label, ".rds")))
  saveRDS(wmd_scaled, file = file.path(out_dir, paste0("wmd_scaled_", country_label, ".rds")))
  
  cat("\n[OK] Heterogeneity scores saved in:", out_dir)
  
  return(list(wmd = wmd, wmd_scaled = wmd_scaled))
}


###############################################################
###                   Obtain Entropy Scores                 ###
###############################################################

cluster_ent <- function(dissim_matrix){
  # Get name of dissimilarity matrix and country label
  dissim_name <- deparse(substitute(dissim_matrix))
  country_label <- sub("_dissim$", "", dissim_name)
  
  # Dynamically retrieve PAM result object from global environment
  pam_result <- get(paste0("pam_result_", country_label), envir = .GlobalEnv)
  
  # Extract cluster info
  cluster_labels <- pam_result$clustering
  cluster_medoids <- pam_result$medoids
  unique_clusters <- unique(cluster_labels)
  k = length(unique_clusters)
  
  # 1. Transversal entropy scores
  ## Get sequence information
  seqact_ch <- get(paste0(country_label, "_seqact_ch"), envir = .GlobalEnv)
  seqact_emp <- get(paste0(country_label, "_seqact_emp"), envir = .GlobalEnv)
  
  ## Initialize lists to store entropy per cluster
  entropy_ch_list <- list()
  entropy_emp_list <- list()
  
  ## Loop over clusters
  for (cl in unique_clusters) {
    cl_idx <- which(cluster_labels == cl)
    
    cl_seq_ch <- seqact_ch[cl_idx, ]
    cl_seq_emp <- seqact_emp[cl_idx, ]
    
  ## Compute transversal entropy (returns a vector, one value per time point)
    ent_ch_vec <- seqstatd(cl_seq_ch)
    ent_emp_vec <- seqstatd(cl_seq_emp)
    
  ## Average across time points for a single entropy value per cluster
    entropy_ch_list[[as.character(cl)]] <- mean(ent_ch_vec$Entropy, na.rm = TRUE)
    entropy_emp_list[[as.character(cl)]] <- mean(ent_emp_vec$Entropy, na.rm = TRUE)
  }
  
  ## Convert into dataframe
  entropy_ch_df <- data.frame(
    cluster = as.integer(names(entropy_ch_list)),
    entropy = unlist(entropy_ch_list)
  )
  
  entropy_emp_df <- data.frame(
    cluster = as.integer(names(entropy_emp_list)),
    entropy = unlist(entropy_emp_list)
  )
  
  
  ## Plot transversal entropy
  par(mfrow = c(2, 4), mar = c(3, 3, 2, 1), oma = c(4, 4, 4, 2))
  
  for (cl in unique_clusters) {
    # Employment cluster plot
    cl_idx <- which(cluster_labels == cl)
    seqHtplot(seqact_emp[cl_idx, , drop = FALSE], 
              main = paste("Employment - Cluster", cl),
              xtlab = 18:50, xstep = 1, cex.axis = 0.7, cex.main = 0.9)
  }
  
  for (cl in unique_clusters) {
    # Family status cluster plot
    cl_idx <- which(cluster_labels == cl)
    seqHtplot(seqact_ch[cl_idx, , drop = FALSE],
              main = paste("Family Status - Cluster", cl),
              xtlab = 18:50, xstep = 1, cex.axis = 0.7, cex.main = 0.9)
  }
  
  # Add common x- and y-axis labels using outer margins
  mtext("Sequence Position", side = 1, outer = TRUE, line = 2, cex = 1)
  mtext("Transversal entropy score", side = 2, outer = TRUE, line = 2, cex = 1)
  
  # Save results
  out_dir <- "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Cluster Heterogeneity/"
  file_name <- paste0(country_label, "_entropy_k",k,".png")
  entropy_filename <- file.path(out_dir, file_name)
  
  png(entropy_filename, width = 1200, height = 800, res = 150)
  
  par(mfrow = c(2, 4), mar = c(3, 3, 2, 1), oma = c(4, 4, 4, 2))
  
  for (cl in unique_clusters) {
    # Employment cluster plot
    cl_idx <- which(cluster_labels == cl)
    seqHtplot(seqact_emp[cl_idx, , drop = FALSE], 
              main = paste("Employment - Cluster", cl),
              xtlab = 18:50, xstep = 1, cex.axis = 0.7, cex.main = 0.9)
  }
  
  for (cl in unique_clusters) {
    # Family status cluster plot
    cl_idx <- which(cluster_labels == cl)
    seqHtplot(seqact_ch[cl_idx, , drop = FALSE],
              main = paste("Family Status - Cluster", cl),
              xtlab = 18:50, xstep = 1, cex.axis = 0.7, cex.main = 0.9)
  }
  
  # Add common x- and y-axis labels using outer margins
  mtext("Sequence Position", side = 1, outer = TRUE, line = 2, cex = 1)
  mtext("Transversal entropy score", side = 2, outer = TRUE, line = 2, cex = 1)
  
  # Close PNG device 
  dev.off()
  cat("\n[OK] Entropy plots saved in:", entropy_filename)
  
  return(list(entropy_ch = entropy_ch_df,
              entropy_emp = entropy_emp_df))
  
}


