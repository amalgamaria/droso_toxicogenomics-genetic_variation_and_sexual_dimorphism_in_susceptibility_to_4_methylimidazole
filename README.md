
## README.md for "Drosophila Toxicogenomics: Genetic Variation and Sexual Dimorphism in Susceptibility to 4-methylimidazole."

## **File Descriptions**

CSB_dose_response_graph.R
- R code for the dose response curve using the CSB data

DGRP2_range_finding.R
- R code for the range finding plot using the DGRP2 data

DGRP3_cumulative_combined_sex_graph.R
- R code for the combined sex line graph using the DGRP3 data 

DGRP3_cumulative_separate_sex_graph.R
- R code for the separate sex line graph using the DGRP3 data

Reaction_norm_graph.R
- R code for the reaction norm graph using the DGRP3 data

database_connections.txt
- Database file needed to run "mccp_network_connectivity.R"

mcpp_network_connectivity.R
- Make sure the R package igraph is installed and usable before running if not running in RStudio
- Run in a new directory and on R version 4.1.2 with the following command: 
- Rscript mcpp_network_connectivity.R <path_to_database_file> <path_to_observed_network_file> <number_of_iterations>
    - database file is provided (database_connections.txt")
    - for the observed network file, have a single column list of genes with a header in .txt format
    - the number of permutations can be any integer, but 10,000 is recommended.
- To run in RStudio, replace "database_file", "observed_list", and "n_permutations" in the main function with paths to your inputs and the number of permutations you will run.
- You will also need to comment out lines 69, 83, and 86-93 if running in RStudio.

[GWA Analysis Pipeline](https://github.com/vshanka23/dgrp_gwas_final), by [@vshanka23](https://github.com/vshanka23)
