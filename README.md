## Information to reproduce the experiments

### Folder Structure

This folder contains the supplementary code for the paper "REDS: Rule Extraction for Discovering Scenarios". Namely,
1. package "reds_0.14.tar.gz" containing the implementation of our method, competing methods, and the quality metrics;
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

### Obtaining the results

The results will appear in the folders "plots_tables" in each subdirectory we refer to.

To obtain Figures 6-12, 14, Tables 3-4, and the results of the Wilcoxon-Mann-Whitney test, execute sequentially in the folders "continuous" and "mixed_ssl":
* "do_experiments.R"
* "analyze_experiments.R"
* "plots_and_tables.R"
* "additional_analysis.R"

To obtain Table 5 and Figure 13, execute sequentially in the folder "third_party":
* "get_data.bat" - this script downloads the "TGL" and "lake" datasets
* "do_experiments.R"
* "analyze_experiments.R"
* "plots_and_tables.R"

To obtain column "share" of Table 2, execute "dgp_analysis.R" in the folder "continuous"
