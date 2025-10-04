# 0.4_clusterDiagnostics.R                                                         

# Evaluation and diagnostics of clusters, it includes:
#   Quality curves across a vector of k (silhouette, WSS, pseudo-R^2, entropy)
#   Optional per-cluster summaries for one chosen k (size, avg-silhouette, 
#     turbulence/complexity, modal sequence, covariate cross-tabs).
# ─────────────────────────────────────────────────────────────────────────────

cluster_diagnostics <- function(
    agnes_object,                 # the agnes() result
    diss,                         # matching dissimilarity matrix (class 'dist' or matrix)
    ks              = 1:10,       # vector of k to scan for the quality curve
    k_descriptive    = NULL,      # single k for per-cluster deep dive
    seq_objects      = NULL,      # named list of seqdef objects (same order as diss)
    data_frame       = NULL,      # raw data (rows must match diss!)
    covariates       = NULL,      # character vector of column names to cross-tab
    save_dir         = "Cluster_Diagnostics", # folder for csv / png
    save_plots       = TRUE,
    save_csv         = TRUE) {
  
  ## ── 0. Load required packages ───────────────────────────────────────────
  pkgs <- c("TraMineR", "cluster", "dplyr", "tibble", "ggplot2")
  lapply(pkgs, function(p) {if (!requireNamespace(p, quietly = TRUE)) install.packages(p);
    library(p, character.only = TRUE)})
  
  # Optional WeightedCluster for Γ; run only if present
  has_WC <- requireNamespace("WeightedCluster", quietly = TRUE)
  if (has_WC) library(WeightedCluster)
  
  ## ── 1. Prep paths / objects ─────────────────────────────────────────────
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  agnes_name <- deparse(substitute(agnes_object))
  
  # Ensure diss is matrix (needed for entropy / WSS math)
  if (inherits(diss, "dist")) diss_mat <- as.matrix(diss) else diss_mat <- diss
  n <- nrow(diss_mat)
  
  ## helper: within-cluster dissimilarity (sum of squared distances) ----------
  wss_fun <- function(cl_vec) {
    sum(vapply(split(seq_len(n), cl_vec), function(idx) {
      if (length(idx) <= 1) return(0)
      mean(diss_mat[idx, idx]^2) * length(idx)
    }, numeric(1)))
  }
  
  ## helper: entropy (Shannon, normalised 0–1) --------------------------------
  ent_fun <- function(cl_vec) {
    p <- prop.table(table(cl_vec))
    if (length(p) == 1) return(0)
    entropy <- -sum(p * log(p))
    entropy / log(length(p))                  # normalised
  }
  
  ## ── 2. QUALITY CURVES ACROSS ks ──────────────────────────────────────────
  if (!is.null(ks) && length(ks) > 0) {
    qual_tbl <- purrr::map_dfr(ks, function(k) {
      clu <- factor(cutree(agnes_object, k))
      sil <- cluster::silhouette(clu, diss_mat)
      
      # inertia
      wss <- wss_fun(clu)
      tss <- mean(diss_mat^2) * n            # total (constant across k)
      r2  <- if (tss > 0) 1 - wss / tss else NA
      
      tibble::tibble(k          = k,
                     Silhouette = mean(sil[, "sil_width"]),
                     WSS        = wss,
                     R2         = r2,
                     Entropy    = ent_fun(clu),
                     Hubert_G   = if (has_WC) {
                       WeightedCluster::wcDistClustStat(diss_mat, clu)$HubertGamma
                     } else NA_real_)
    })
    
    if (save_csv) {
      readr::write_csv(qual_tbl,
                       file.path(save_dir, paste0(agnes_name, "_quality_curve.csv")))
    }
    
    if (save_plots) {
      p1 <- ggplot2::ggplot(qual_tbl, ggplot2::aes(k, WSS)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::labs(title = paste("Elbow –", agnes_name),
                      x = "k", y = "Within-cluster SS (OM²)") +
        ggplot2::theme_minimal()
      p2 <- ggplot2::ggplot(qual_tbl, ggplot2::aes(k, Silhouette)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::labs(title = paste("Average silhouette –", agnes_name),
                      x = "k", y = "Silhouette") +
        ggplot2::theme_minimal()
      ggplot2::ggsave(file.path(save_dir, paste0(agnes_name, "_elbow.png")), p1, 6,4)
      ggplot2::ggsave(file.path(save_dir, paste0(agnes_name, "_silhouette.png")), p2, 6,4)
      print(p1); print(p2)
    }
  } else {
    qual_tbl <- NULL
  }
  
  ## ── 3. PER-CLUSTER DETAILS FOR CHOSEN k ──────────────────────────────────
  if (!is.null(k_descriptive)) {
    clu <- factor(cutree(agnes_object, k_descriptive), levels = 1:k_descriptive)
    
    # basic sizes & silhouette per cluster
    sil <- cluster::silhouette(clu, diss_mat)
    sil_by_cl <- tapply(sil[, "sil_width"], clu, mean)
    
    size_tbl <- tibble::tibble(cluster        = levels(clu),
                               n              = as.vector(table(clu)),
                               avg_silhouette = as.numeric(sil_by_cl))
    
    # turbulence / complexity (if sequences supplied)
    if (!is.null(seq_objects)) {
      for (nm in names(seq_objects)) {
        turb <- TraMineR::seqST(seq_objects[[nm]])
        size_tbl[[paste0("Turb_", nm)]] <- tapply(turb, clu, mean)
      }
    }
    
    # modal sequence (first seq object) if provided
    if (!is.null(seq_objects) && length(seq_objects) > 0) {
      first_seq <- seq_objects[[1]]
      modal <- vapply(levels(clu), function(cl) {
        TraMineR::seqmod(first_seq[clu == cl, ], weighted = FALSE, plabel = FALSE)$mseq[1]
      }, character(1))
      size_tbl$Modal <- modal
    }
    
    # covariate cross-tabs (if data provided)
    covar_tabs <- list()
    if (!is.null(data_frame) && !is.null(covariates)) {
      if (nrow(data_frame) != n) stop("data_frame rows must match diss rows")
      for (v in covariates) {
        var <- as.character(data_frame[[v]])
        tab <- prop.table(table(clu, var), 2) * 100
        tab_df <- as.data.frame.matrix(tab)
        covar_tabs[[v]] <- tab_df
        if (save_csv) {
          utils::write.csv(tab_df,
                           file = file.path(save_dir, sprintf("%s_k%d_crosstab_%s.csv", agnes_name, k_descriptive, v)),
                           row.names = TRUE)
        }
      }
    }
    
    if (save_csv) {
      utils::write.csv(size_tbl,
                       file = file.path(save_dir, sprintf("%s_k%d_cluster_summary.csv", agnes_name, k_descriptive)),
                       row.names = FALSE)
    }
    
    assign(paste0(agnes_name, "_k", k_descriptive, "_summary"), size_tbl, envir = .GlobalEnv)
    if (length(covar_tabs)) assign(paste0(agnes_name, "_k", k_descriptive, "_covars"), covar_tabs, envir = .GlobalEnv)
  }
  
  invisible(list(curves = qual_tbl,
                 summary = if (exists("size_tbl")) size_tbl else NULL))
}
