#' Pairwise Invasibility Plot
#'
#' This function plots a PIP for a given combination of parameters beta (choice parameter) and s (selection coefficient).
#'
#' @param path Path to where data for PIP are stored.
#' @param beta Choice parameter value.
#' @param s Selection coefficient value.
#' @author Raphael Scherrer
#' @export

# Function to plot a PIP
plot_pip <- function(path, beta, s) {

  homedir <- getwd()
  setwd(path)

  # Record files
  allfiles <- list.files()

  splitfiles <- strsplit(gsub(".csv", "", allfiles), "_")

  # Where in the file name to look for parameter values?
  idbeta <- grep("beta", splitfiles[[1]]) + 1
  ids <- grep("^s$", splitfiles[[1]]) + 1

  # Parameter values
  betas <- as.numeric(sapply(splitfiles, "[", idbeta))
  ss <- as.numeric(sapply(splitfiles, "[", ids))

  # Find the right file
  filename <- allfiles[betas == beta & ss == s]
  if(length(filename) > 1) stop("there is more than one files for this parameter set")
  if(length(filename) == 0) stop("there is no file for this parameter set")

  # Load the file
  pip <- read.csv(filename, header = F)

  # What is the resolution?
  idgrain <- grep("grain", splitfiles[[1]]) + 1
  grain <- as.numeric(splitfiles[[1]][idgrain])

  # Set the ranges of x and y values
  xs <- ys <- seq(-1, 1, grain)

  # Transfor the PIP matrix into a ggplot compatible table
  # Note: x are in columns, y in rows in the matrix
  ggpip <- cbind(expand.grid(xs, ys), unlist(lapply(seq_len(nrow(pip)), function(i) pip[i,])))
  colnames(ggpip) <- c("x", "y", "fitness")
  ggpip <- as.data.frame(ggpip)

  # Plot the heatmap
  library(ggplot2)
  p <- ggplot(data = ggpip, mapping = aes(x = x, y = y, fill = fitness))
  p <- p + geom_tile()
  p <- p + theme_bw()
  p <- p + xlab(label = "Resident phenotype")
  p <- p + ylab(label = "Mutant phenotype")
  print(p)

  setwd(homedir)

}
