# 0.3_clusterSelection.R

# Determine the optimal number of clusters for a TraMineR/agnes result
# using the Elbow (WSS) and Average-Silhouette criteria, and save the
# diagnostic plots alongside the rest of our outputs.
###############################################################################

###############################################################################
cluster_selection <- function(agnes_object,
                              max_k      = 10,
                              diss       = NULL,
                              palette    = NULL,
                              save_plots = TRUE,
                              title      = NULL) {
  
  ## 0. Install / load required libraries
  required_pkgs <- c("cluster", "ggplot2", "dplyr", "stringr", "scales", "WeightedCluster")
  lapply(required_pkgs, function(p) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
    library(p, character.only = TRUE)
  })
  
  ## 1. HOUSE-KEEPING
  agnes_name  <- deparse(substitute(agnes_object))          # e.g. "france_agnes_man"
  # base_label  <- sub("_agnes.*$", "", agnes_name)           # "france"
  # if (is.null(title))
  #   title <- str_to_title(gsub("_", " ", base_label))       # default: "France"
  
  ## little modification to include the sex in the plot title
  base_label <- sub("_agnes", "", agnes_name)               # "denmark", "denmark_man", "denmark_woman"
  base_label <- gsub("_", " ", base_label)                  # "denmark man"
  country_label <- sub("_agnes.*$", "", agnes_name)
  
  
  if (is.null(title))
    title <- str_to_title(base_label)                       # "Denmark Man", "Denmark Woman"
  
  # Where to store the png files
  output_dir <- file.path("N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Cluster Diagnostics", country_label)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  ## 2. DISSIMILARITY MATRIX 
  if (is.null(diss) && !is.null(agnes_object$diss)) diss <- agnes_object$diss
  if (is.null(diss)) {
    diss_name <- paste0(base_label, "_dissim")
    if (!exists(diss_name, envir = .GlobalEnv))
      stop("Cannot find dissimilarity matrix. Pass it via 'diss=' or ",
           "make sure '", diss_name, "' exists in the global environment.")
    diss <- get(diss_name, envir = .GlobalEnv)
  }
  diss_mat <- as.matrix(diss)
  
  ## 3. LOOP OVER k AND COLLECT METRICS 
  ks     <- 2:max_k
  wss    <- silavg <- numeric(length(ks))
  
  for (idx in seq_along(ks)) {
    k   <- ks[idx]
    clu <- cutree(agnes_object, k)
    
    silobj      <- cluster::silhouette(clu, diss_mat)
    silavg[idx] <- mean(silobj[, "sil_width"])
    
    wss[idx] <- sum(vapply(unique(clu), function(cl) {
      mem <- which(clu == cl)
      mean(diss_mat[mem, mem]) * length(mem)
    }, numeric(1)))
  }
  metrics <- data.frame(k = ks, WSS = wss, Silhouette = silavg)
  
  ## 4. VISUALISATION
  if (is.null(palette)) palette <- hue_pal()(2)
  
  p_elbow <- ggplot(metrics, aes(k, WSS)) +
    geom_line(colour = palette[1]) +
    geom_point(colour = palette[1]) +
    labs(title = paste(title, "- Elbow plot"),
         x = "Number of clusters (k)",
         y = "Within-cluster dissimilarity (OM-based)") +
    theme_minimal(base_size = 12)
  
  p_sil <- ggplot(metrics, aes(k, Silhouette)) +
    geom_line(colour = palette[2]) +
    geom_point(colour = palette[2]) +
    labs(title = paste(title, "- Average silhouette width"),
         x = "Number of clusters (k)", y = "Silhouette") +
    theme_minimal(base_size = 12)
  
  ## 5. SAVE & RETURN (unchanged except filenames already include object name)
  if (save_plots) {
    ggsave(file.path(output_dir, paste0(agnes_name, "_elbow.png")),
           p_elbow, width = 6, height = 4, dpi = 300)
    ggsave(file.path(output_dir, paste0(agnes_name, "_silhouette.png")),
           p_sil,  width = 6, height = 4, dpi = 300)
    message("✔ Diagnostics saved to: ", output_dir)
  }
  
  print(p_elbow); print(p_sil)
  invisible(metrics)
}

###############################################################################
## ASW graphs
###############################################################################
run_cluster_diagnostics <- function(agnes_obj,
                                    diss_obj,
                                    label,
                                    title = NULL) {
  
  agnes_name  <- deparse(substitute(agnes_object))          # e.g. "france_agnes_man"
  country_label <- sub("_agnes.*$", "", agnes_name)
  
  
  if (is.null(title))
    title <- stringr::str_to_title(gsub("_", " ", label))   
  
  wardRange <- as.clustrange(agnes_obj, diss = diss_obj)
  summary(wardRange, max.rank = 5)
  
  ## On-screen plot
  plot(wardRange, stat = c("ASW", "R2", "CH"),
       norm = "zscore", main = title)
  
  output_path <- paste0(
    "N:/Incubator2025_ComputationalLifeCourse/Analysis/Sequence + Cluster Analysis/Cluster Diagnostics/",
    country_label, "_", label, "_asw.png"
  )

  png(filename = output_path, width = 1000, height = 800, res = 150)
  plot(wardRange, stat = c("ASW", "R2", "CH"),
       norm = "zscore", main = title)
  dev.off()
  message("Saved: ", output_path)
  
  invisible(wardRange)
}
