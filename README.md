## Information to reproduce the experiments

### Folder Structure

This folder contains the supplementary code for the paper "REDS: Rule Extraction for Discovering Scenarios". Namely,
1. package "reds_0.12.tar.gz" containing the implementation of our method, competing methods, and the quality metrics;
2. files ".R" and "RData" containing the code and data for reproducing all experiments and respective plots and tables presented in the paper.


### Required packages

In our experiments we used the following other packages, available at CRAN repository (https://cran.r-project.org/)
* caret (Version: 6.0-86)
* randomForest (Version: 4.6-14)
* sensitivity (Version: 1.22.0)
* lhs (Version: 1.0.2)
* stats (Version: 4.0.1)
* batchtools (Version: 0.9.13)
* data.table (Version: 1.13.0)

For producing the plots we additionally used:
* reshape (Version: 0.8.8)
* ggplot2 (Version: 3.3.2)
* gridExtra (Version: 2.3)
* RColorBewer (Version: 1.1-2)


### How to install and execute the code

The rest of the description assumes that one uses RStudio (https://www.rstudio.com/). Any of the above packages can be installed with the command
`install.packages("<package-name>")`

To install our package, execute
`install.packages("<path-to-file>", repos = NULL, type = "source")`

To repeat our experiments in each folder, open the file ".RProj" and open all files ".R" from that folder. To launch part of code, select it and press "Ctrl + Enter".

### The folder "Experiments"

The results will appear in the folder "plots_tables"

To obtain Figures 4-10 and Tables 3-4, execute **sequentially** 
	* "do_experiments.R" (takes ~48 hours on 16 cores)
	* "analyze_experiments.R" (~30 minutes on 1 core) and
	* "plots_and_tables.R" (~2 minutes on 1 core)

To obtain Table 2, execute "dgp_analysis.R" 
