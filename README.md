## Information to reproduce the experiments

### Repository Structure

This folder contains the supplementary code for the paper 

Vadim Arzamasov, Klemens Böhm. 2021. REDS: Rule Extraction for Discovering
Scenarios. In Proceedings of the 2021 International Conference on
Management of Data (SIGMOD ’21), June 20–25, 2021, Virtual Event, China.
ACM, NewYork, NY, USA, 14 pages. https://doi.org/10.1145/3448016.3457301. 

Namely,

1. package "reds_0.14.tar.gz" containing the implementation of REDS, PRIM, and BI algorithms, the quality metrics, and the functions we experiment with. 
This version corresponds to the commit 12da8bd1b535169975aa686d4bdf4015d3a46563 in the [reds repository](https://github.com/Arzik1987/reds);
2. files ".R" and "RData" containing the code and data for reproducing all experiments and respective plots and tables presented in the paper;
3. files ".bat" that download from other repositories on GitHub the third-party datasets and code to analyze the results. On a non-Windows machine, you need to execute the commands from this file manually.


### Hardware/Software

We used a virtual machine with 32 cores at 2GHz (AMD EPYC 7551) and 128GB of memory operated by Linux 5.4.0. 

In our experiments, we used [R version 3.6.3](https://cran.r-project.org/bin/windows/base/old/3.6.3/) 

You will also need [Rtools35](https://cran.rstudio.com/bin/windows/Rtools/history.html) to install specific versions of packages.

For convenience, we recommend using [RStudio](https://www.rstudio.com/products/rstudio/download/#download). In RStudio, to execute part of code, one selects it and presses `Ctrl + Enter`.  


### Getting ready to execute experiments


* In each folder, run the file `.bat` (or execute the commands from this file manually on non-Windows machines)
* navigate to the folder `install_requirements`,
* (if using RStudio) open the files  `install_requirements.RProj` and `install_requirements.R` in this sequence


### Obtaining figures and tables 

The results will appear in the folders `main/plots_tables` and `third_party/plots_tables`.

To obtain Figures 6-12, 14, Tables 3-4, and the results of the statistical tests, do:

* navigate to the folder `main`,
* (if using RStudio) open the file `experiments_main.RProj` and all files `.R` in this sequence,
* `do_experiments.R`,
* `analyze_experiments.R`,
* `plots_and_tables.R`.

To obtain column "share" of Table 2, execute `dgp_analysis.R`

To obtain Table 5 and Figure 13, do:

* navigate to the folder `third_party`,
* (if using RStudio) open the file `experiments_tp.RProj` and all files `.R` in this sequence,
* `do_experiments.R`,
* `analyze_experiments.R`,
* `plots_and_tables.R`.
