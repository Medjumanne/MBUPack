library(roxygen2)

#' Whittaker Plot
#'
#' This function generates a Whittaker Plot, which is a type of rank-abundance plot used in ecology to display relative species abundance, the number of species, and species richness in a community.
#'
#' @param data A data frame containing the species abundance data. Each row represents a species, and each column represents a location. The column names should be in the format of the proportions of each location.
#' @param sites A character vector of location names. These should match the location names in the data frame .
#' @param sppMax The upper limit for the x-axis (species rank).
#' @param ylog A logical value indicating whether the y-axis should be on a log scale or not. The default value is TRUE.
#'
#' @return A Whittaker Plot is displayed.
#' @importFrom graphics lines
#' @importFrom graphics legend
#' @export
#' @examples
#' # Example data
#' transposed_data <- data.frame(
#' Species = c("Species1", "Species2", "Species3", "Species4", "Species5"),
#' Site1 = c(5, 3, 0, 7, 2),
#' Site2 = c(2, 0, 6, 1, 4),
#' Site3 = c(1, 8, 2, 0, 5),
#' Site4 = c(7, 1, 3, 4, 6),
#' Site5 = c(3, 2, 5, 6, 1),
#' Site6 = c(4, 5, 1, 2, 3),
#' Site7 = c(6, 4, 7, 3, 0)
#)
# Use the WhittakerPlot function
WhittakerPlot(transposed_data, "Site1, Site2, Site3, Site4, Site5, Site6, Site7", 26, ylog = FALSE)

WhittakerPlot <- function(data, sites, sppMax, ylog = TRUE) {
  # Define colors and shapes for up to 20 sites
  colors <- c("red", "blue", "black", "darkred", "green", "purple", "brown", "pink", "orange", "cyan",
              "magenta", "yellow", "gray", "darkgreen", "darkblue", "darkgray", "lightblue", "lightgreen", "lightgray", "lightpink")
  shapes <- c(1:20)
  
  # Convert sites to a vector if it's a comma-separated string
  if (is.character(sites) && grepl(",", sites)) {
    sites <- unlist(strsplit(sites, ","))
  }
  
  # Trim any leading or trailing white space from the site names
  sites <- trimws(sites)
  
  # Order the data for each site by abundance
  ordered_data <- lapply(sites, function(site) {
    if (site %in% names(data)) {
      data[order(-data[[site]]),]
    } else {
      stop(paste("site", site, "not found in data"))
    }
  })
  
  # Plot the data for the first site
  plot(ordered_data[[1]][[sites[1]]], type = "o", xlim = c(1, sppMax), lwd = 2,
       col = colors[1], pch = shapes[1], cex = 2, xlab = "Rank", ylab = "Relative Abundance",
       main = "Whittaker Plot", log = ifelse(ylog, "y", ""))
  
  # Add lines for the other sites
  for (i in 2:length(sites)) {
    lines(ordered_data[[i]][[sites[i]]], type = "o", lwd = 2, col = colors[i], pch = shapes[i])
  }
  
  # Add legend with reduced size
  legend("topright", legend = sites, col = colors[1:length(sites)], pch = shapes[1:length(sites)], lwd = 2, title = "Site", cex = 0.8)
}
