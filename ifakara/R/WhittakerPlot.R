library(roxygen2)

#' Whittaker Plot
#'
#' This function generates a Whittaker Plot, which is a type of rank-abundance plot used in ecology to display relative species abundance, the number of species, and species richness in a community.
#'
#' @param data A data frame containing the species abundance data. The first column should represent the taxon, and the rest of the columns represent the sites.
#' @param sites A vector of site names to be included in the plot.
#' @param ylog A logical value indicating whether the y-axis should be on a log scale or not. The default value is FALSE.
#' @param maxRank An optional numeric value specifying the maximum rank for the x-axis. If not specified, the function will use the number of rows in the data.
#'
#' @return A Whittaker Plot is displayed.
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @examples
#' # Example data
#' transposed_data <- data.frame(
#' Species = c("Species1", "Species2", "Species3", "Species4", "Species5"),
#' Site1 = c(5, 3, 0, 7, 2),
#' Site2 = c(2, 0, 6, 1, 4),
#' Site3 = c(1, 8, 2, 0, 5),
#' Site4 = c(7, 1, 3, 4, 6),
#' Site5 = c(3, 2, 5, 6, 1)
#' )
#' # Use the WhittakerPlot function
#' WhittakerPlot(transposed_data, c("Site1", "Site5"))
#' @export
#' 

WhittakerPlot <- function(data, sites, ylog = FALSE, maxRank = nrow(data)) {
  # Define colors and shapes for up to 20 sites
  colors <- c("red", "blue", "black", "darkred", "green", "purple", "brown", "pink", "orange", "cyan",
              "magenta", "yellow", "gray", "darkgreen", "darkblue", "darkgray", "lightblue", "lightgreen", "lightgray", "lightpink")
  shapes <- c(1:20)
  
  # Order the data for each site by abundance
  ordered_data <- lapply(sites, function(site) {
    data[order(-data[[site]]),]
  })
  
  # Set the y-axis label based on whether ylog is TRUE or FALSE
  ylab <- ifelse(ylog, "Relative Abundance (log)", "Relative Abundance")
  
  # Plot the data for the first site
  plot(ordered_data[[1]][[sites[1]]], type = "o", xlim = c(1, maxRank), ylim = c(0, 1), lwd = 2,
       col = colors[1], pch = shapes[1], cex = 2, xlab = "Rank", ylab = ylab,
       main = "Whittaker Plot", log = ifelse(ylog, "y", ""))
  
  # Add lines for the other sites
  for (i in 2:length(sites)) {
    lines(ordered_data[[i]][[sites[i]]], type = "o", lwd = 2, col = colors[i], pch = shapes[i])
  }
  
  # Add legend with reduced size
  legend("topright", legend = sites, col = colors[1:length(sites)], pch = shapes[1:length(sites)], lwd = 2, title = "Site", cex = 0.8)
}
