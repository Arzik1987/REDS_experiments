# first install Rtools from https://cran.rstudio.com/bin/windows/Rtools/Rtools35.exe

install.packages("remotes")
require(remotes)

# the versions of the following four are approximate, but should not be very important
install_version("prim", version = "1.0.19", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("kernlab", version = "0.9-29", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("xgboost", version = "1.1.1.1", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("e1071", version = "1.7-3", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install.packages("snow")

# these versions are exact
install_version("caret", version = "6.0-86", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("randomForest", version = "4.6-14", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("sensitivity", version = "1.22.0", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("lhs", version = "1.0.2", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("batchtools", version = "0.9.13", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("data.table", version = "1.13.0", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("logitnorm", version = "0.8.38", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)

install_version("reshape", version = "0.8.8", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("ggplot2", version = "3.3.2", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("gridExtra", version = "2.3", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("RColorBrewer", version = "1.1-2", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)
install_version("ggbeeswarm", version = "0.6.0", repos = "http://cran.us.r-project.org", upgrade = "never", quiet = TRUE)

install.packages("../reds_0.14.tar.gz", repos = NULL, type = "source")

# # Remove all extra packages
# # see https://www.r-bloggers.com/2016/10/how-to-remove-all-user-installed-packages-in-r/
# # create a list of all installed packages
# ip <- as.data.frame(installed.packages())
# head(ip)
# # if you use MRO, make sure that no packages in this library will be removed
# ip <- subset(ip, !grepl("MRO", ip$LibPath))
# # we don't want to remove base or recommended packages either\
# ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# # determine the library where the packages are installed
# path.lib <- unique(ip$LibPath)
# # create a vector with all the names of the packages you want to remove
# pkgs.to.remove <- ip[,1]
# head(pkgs.to.remove)
# # remove the packages
# sapply(pkgs.to.remove, remove.packages, lib = path.lib)