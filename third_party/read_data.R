d <- read.csv("original\\bryant_et_al_2010_data.csv")
dTGL <- d[, c(3:11,16)]
colnames(dTGL) <- c("bpc", "lcbs", "fsd", "by", "ose", "tde", "ec", "sosc", "bbp", "y")

d <- read.csv("original\\experiments.csv")
d <- d[, c("b", "delta", "mean", "q", "stdev", "policy")]
d1 <- read.csv("original\\max_P.csv", header = FALSE)
dl <- list(d[d$policy == 0, 1:5],as.numeric(d1[d$policy == 0,] < 1))

rm(d)
rm(d1)
