#' Calculate Diversity Indices for Each Site
#'
#' @param data A data frame where each row represents a species (taxon) and each 
#'   column represents a site. The first column should be the species names, and 
#'   the rest of the columns should be the counts of each species at each site.
#'
#' @return A data frame where each row corresponds to a site and the columns 
#'   represent the Shannon Index, Simpson Index, Inverse Simpson Index, and Species Richness.
#'
#' @examples
#' # Hypothetical data
#' data <- data.frame(
#'   Taxon = c("Species1", "Species2", "Species3", "Species4"),
#'   Site1 = c(5, 3, 0, 7),
#'   Site2 = c(2, 0, 6, 1),
#'   Site3 = c(1, 8, 2, 0),
#'   Site4 = c(7, 1, 3, 4)
#' )
#'
#' # Calculate the diversity indices for all sites
#' diversity_indices <- dmima(data)
#'
#' # Print the diversity indices
#' print(diversity_indices)
#'
#' @export
dmima <- function(data) {
  # Get the unique sites
  sites <- colnames(data)[-1]
  
  # Initialize an empty data frame to store the results
  results <- data.frame()
  
  # Loop over each site
  for (site in sites) {
    # Subset the data for the current site
    subset_data <- data[,c(1, which(colnames(data) == site))]
    
    # Calculate the diversity indices
    shannon_index <- vegan::diversity(subset_data[,2])
    simpson_index <- vegan::diversity(subset_data[,2], index = "simpson")
    invsimpson_index <- vegan::diversity(subset_data[,2], index = "invsimpson")
    richness <- vegan::specnumber(subset_data[,2])
    
    # Create a data frame to store the results for the current site
    result <- data.frame(
      Site = site,
      Shannon_Index = shannon_index,
      Simpson_Index = simpson_index,
      InvSimpson_Index = invsimpson_index,
      Richness = richness
    )
    
    # Append the result to the results data frame
    results <- rbind(results, result)
  }
  
  # Return the results
  return(results)
}
