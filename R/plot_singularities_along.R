#' Plot evolutionary singularities
#'
#' This function plots the singularities of the adaptive dynamics along one parameter, either the selection coefficient (s) or the choice parameter (beta). Data are subset by the other parameter.
#'
#' @param path Path to where data for singularities are stored.
#' @param var Parameter along which to plot singularities.
#' @param subset_by Parameter for subsetting.
#' @param subset_value Value of the subset parameter.
#' @author Raphael Scherrer
#' @export

# Function to plot singularities along a parameter
plot_singularities_along <- function(path = ".", var = "s", subset_by = "beta", subset_value = 0) {

  homedir <- getwd()
  setwd(path)

  # Get subset parameter values
  singularity_files <- list.files()[grep("sing", list.files())]
  subset_values <- as.numeric(sapply(strsplit(singularity_files, "_"), function(curr_filename) curr_filename[grep(paste0("^", subset_by, "$"), curr_filename) + 1]))

  # Read the files
  singularities <- lapply(singularity_files[subset_values == subset_value], read.csv, header = F)

  # Get plotting parameter values
  parameter_values <- as.numeric(sapply(strsplit(singularity_files, "_"), function(curr_filename) curr_filename[grep(paste0("^", var, "$"), curr_filename) + 1]))[subset_values == subset_value]

  # Reformat
  parameter_values <- unlist(mapply(function(curr_paramvalue, curr_singularity) {
    rep(curr_paramvalue, nrow(curr_singularity))
  }, parameter_values, singularities))
  singularities <- do.call("rbind", singularities)

  # Color according to stability
  stability <- as.factor(apply(singularities[,-1], 1, paste, collapse = ":"))

  # Plot
  plot(parameter_values, singularities[,1], col = stability, pch = 16, ylab = "Ecological phenotype", xlab = ifelse(var == "s", "Selection coefficient", "Choice parameter"), las = 1)
  legend(x = 1.2, y = 0.8, legend = c("Repeller", "Branching point", "Stable strategy"), pch = 16, col = c("black", "red", "green3", "blue"), bg = gray(0.9))

  setwd(homedir)

}

