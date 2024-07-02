#!/usr/bin/env Rscript

#The database file is provided and named "database_connections.txt"
# for the observed network file, have a single column list of genes with a header in .txt format
# the number of permutations can be any integer, but 10,000 is recommended.

#Keep script unchanged. On Linux, run in a new directory and on R version 4.1.2 with the following command: 
# Rscript mcpp_network_connectivity.R <path_to_database_file> <path_to_observed_network_file> <number_of_iterations>

#To run in RStudio, replace "database_file", "observed_list", and "n_permutations" in the main function with paths to your inputs and the number of permutations you will run.
# You will also need to comment out lines 69, 83, and 86-93 if running in RStudio.

# Install and load igraph
install.packages("igraph")
library(igraph)

# This is the function that calculates the degree of connectivity (connectivity index)
calculate_connectivity_index <- function(graph, vertices) {
  subgraph <- induced_subgraph(graph, vertices)
  return(mean(degree(subgraph)))
}

# This is the main function of the Monte Carlo Permutation. If running in RStudio, replace (database_file, observed_list, n_permutations) with your input.
monte_carlo_permutation <- function(database_file, observed_list, n_permutations) {
  # Read in the interaction data
  interaction_data <- read.delim(database_file, header=TRUE, sep="\t")
  
  # Make a graph object from the interaction data
  g <- graph_from_data_frame(interaction_data, directed=FALSE)
  
  # Read in the observed network data
  observed_data <- read.delim(observed_list, header=TRUE, sep="\t")
  observed_g <- induced_subgraph(g,V(g)[name %in% c(observed_data[,1])])
  
  # Calculate the observed connectivity index
  observed_vertices <- V(observed_g)
  observed_connectivity_index <- calculate_connectivity_index(observed_g, observed_vertices) #<-bug fixed
  
  # Set the sample size based on the number of vertices in the observed network
  sample_size <- length(observed_vertices)
  
  # Create a vector to store the null distribution of connectivity indices
  null_distribution <- numeric(n_permutations)
  
  # Perform the permutations
   for (i in 1:n_permutations) {
      # Sample vertices from the graph
      sampled_names <- sample(c(V(g)$name), sample_size)
      sample_vertices <- V(g)[name %in% sampled_names]
      # Calculate the connectivity index for the sampled sub-graph
      null_distribution[i] <- calculate_connectivity_index(g, sample_vertices)
    }
  
  # Compare the observed connectivity index to the null distribution
  p_value <- sum(null_distribution >= observed_connectivity_index) / n_permutations
  
  # Print the results
  cat("Observed Connectivity Index:", observed_connectivity_index, "\n")
  cat("P-value:", p_value, "\n")

  # Diagnostics: Examine the null distribution
  cat("Summary of null distribution:\n")
  print(summary(null_distribution))
  cat("Standard deviation of null distribution:", sd(null_distribution), "\n")


  # Plot the histogram of the null distribution. Comment out line 69 and 83 if running in RStudio.

  tiff("hist_kde_overlay.tiff", width = 6, height = 6, units = 'in', res = 300)
  hist(null_distribution, breaks=40, main="Histogram with KDE Overlay", xlab="Connectivity Index", ylab="Frequency", col="gray", border="black", freq=FALSE)
  
  # Calculate and plot the KDE with adjusted bandwidth
  kde <- density(null_distribution, bw=0.0125)  # You can adjust the bandwidth method or value here
  lines(kde, col="blue", lwd=2)
  
  # Add observed connectivity index line
  abline(v=observed_connectivity_index, col="red", lwd=2, lty=2)
  
  # Add legend
  legend("topright", legend=c(paste("Observed Index =", round(observed_connectivity_index, 2)), "KDE"), col=c("red", "blue"), lwd=2, lty=2)
  
  # Save the plot to a file 
  dev.off()
}

# Get command line arguments (Comment out from this point on if running in RStudio)
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 3) {
  stop("Please provide three arguments: database_file, observed_list, and n_permutations")
}

# Run the Monte Carlo Permutation Procedure
monte_carlo_permutation(args[1], args[2], as.integer(args[3]))