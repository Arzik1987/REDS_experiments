## Folder Structure
This folder contains the supplementary code for the paper "Scenario Discovery via Rule Extraction". Namely,
1. package "primre_0.11.tar.gz" containing implementation of our method and competing ones, as well as the functions (DGPs) for testing;
2. folder "Experiments" containing the code for reproducing all experiments featured in the paper, except those with MSE;
3. folder "Experiments_bv" containing the code to reproduce experiments with mean squared error (MSE).

All these files will be in open access after the paper acceptance.

## Important note

Anonymization poses the limit on the maximal file size. Because of this, we do not upload the complete DSGC data (one of 33 datasets we used in the study). For this data we only upload the lables; and we use code to generate the inputs. Due to the use of seed it is very likely that the data produced with this code will coincide with the one used in our experiments, but we do not guarantee that. In other words, there is a (small) probability that the results for "dsgc" dataset will be random and should be ignored in this case. The results for the other 32 DGPs are fully reproducible.

This problem will not occur after the paper acceptance, since we will share the complete "dsgc" dataset.

## Required packages

In our experiments we used the following other packages, available at CRAN repository (https://cran.r-project.org/)
* caret (Version: 6.0-81)
* randomForest (Version: 4.6-14)
* sensitivity (Version: 1.15.2)
* lhs (Version: 1.0.1)
* stats (Version: 3.5.1)
* batchtools (Version: 0.9.11)
* data.table (Version: 1.12.0)

For producing the plots we additionally used:
* reshape (Version: 0.8.8)
* ggplot2 (Version: 3.1.0)
* gridExtra (Version: 2.3)
* RColorBewer (Version: 1.1-2)

For this anonymous version we also need (to generate "dscg" data):
* randtoolbox (Version: 1.17.1)

## Additional Information

The rest of description assumes that you use RStudio (https://www.rstudio.com/).
Any of the above packages can be installed with the command
`install.packages("<package-name>")`

To install our package excecute
`install.packages("<path-to-file>", repos = NULL, type = "source")`

To repeat our experiments in each folder open the file ".RProj" and open all files ".R" from that project. To launch part of code, select it and press "Ctrl + Enter".

## The folder "Experiments"

The results will appear in the folder "plots_tables"

0. generate "dsgc" data by excecuting "generate_dsgc.R"
1. To obtain Figure 2, excecute "small data.R"
2. To obtain column "share" of Table 7, execute "dgp_analysis.R"
3. To obtain Figures 5-13 and Tables 2-6, 9-12 execute **sequentially** 
	* "do_experiments.R" (takes ~24 hours on 4 cores)
	* "analyze_experiments.R" (several minutes) and
	* "plots_and_tables.R" (fast)

## The folder "Experiments_bv"

To obtain numbers from Sections 5.2 and Appendix B, as well as the Figure 3, execute **sequentially** 
* "do_experiments.R" and
* "analyze_experiments.R" 