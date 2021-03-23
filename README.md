## Information to reproduce the experiments

### Repository Structure

This folder contains the supplementary code for the paper 

Vadim Arzamasov, Klemens Böhm. 2021. REDS: Rule Extraction for Discovering
Scenarios. In Proceedings of the 2021 International Conference on
Management of Data (SIGMOD ’21), June 20–25, 2021, Virtual Event, China.
ACM, NewYork, NY, USA, 14 pages. https://doi.org/10.1145/3448016.3457301. 

Namely,
1. package "reds_0.14.tar.gz" containing the implementation of REDS, PRIM and BI algorithms, the quality metrics and the functions we experiment with. 
This version corresponds to the commit 12da8bd1b535169975aa686d4bdf4015d3a46563 in the repository https://github.com/Arzik1987/reds;
2. files ".R" and "RData" containing the code and data for reproducing all experiments and respective plots and tables presented in the paper;
3. files ".bat", which download the third-party datasets and code to analalyse the results from other repositories on github.


### Required packages

In our experiments we used the following other packages, available at CRAN repository (https://cran.r-project.org/)
* caret (Version: 6.0-86)
* randomForest (Version: 4.6-14)
* sensitivity (Version: 1.22.0)
* lhs (Version: 1.0.2)
* stats (Version: 4.0.1)
* batchtools (Version: 0.9.13)
* data.table (Version: 1.13.0)
* logitnorm (Version 0.8.38)

For producing the plots we additionally used:
* reshape (Version: 0.8.8)
* ggplot2 (Version: 3.3.2)
* gridExtra (Version: 2.3)
* RColorBewer (Version: 1.1-2)


### How to install and execute the code

The rest of the description assumes that one uses RStudio (https://www.rstudio.com/). To execute part of code, select it and press "Ctrl + Enter". Any of the above packages can be installed with the command
`install.packages("<package-name>")`

To install our package, execute
`install.packages("<path-to-file>", repos = NULL, type = "source")`

To repeat our experiments, in each folder
1. run the file ".bat"
2. open the file ".RProj"
3. open all files ".R" from that folder. 

### Obtaining the results

The results will appear in the folders "plots_tables" in each subdirectory we refer to.

To obtain Figures 6-12, 14, Tables 3-4, and the results of the statistical tests, execute sequentially in the folders "main":
* "do_experiments.R"
* "analyze_experiments.R"
* "plots_and_tables.R"

To obtain Table 5 and Figure 13, execute sequentially in the folder "third_party":
* "do_experiments.R"
* "analyze_experiments.R"
* "plots_and_tables.R"

To obtain column "share" of Table 2, execute "dgp_analysis.R" in the folder "continuous"
