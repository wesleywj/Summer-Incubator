load_required_packages <- function(packages) {
  installed <- packages %in% rownames(installed.packages())
  if (any(!installed)) {
    install.packages(packages[!installed])
  }
  invisible(lapply(packages, library, character.only = TRUE))
}

# Example usage
load_required_packages(c("TraMineR", "dplyr", "colorspace", 
                         "RColorBrewer", "randomcoloR", 
                         "cluster", "Polychrome", "tools"))

