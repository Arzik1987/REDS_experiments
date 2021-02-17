
list.of.packages <- c("reshape", "ggplot2", "gridExtra", "RColorBrewer", "ggbeeswarm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(reshape)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(data.table)
library(batchtools)
library(ggbeeswarm)

dir.create(file.path(getwd(), "plots_tables"), showWarnings = FALSE)

load(paste0(getwd(),"/results/res.RData"))
load(paste0(getwd(),"/results/res_f.RData"))
load(paste0(getwd(),"/results/peeling_dsgc.RData"))
load(paste0(getwd(),"/results/peeling_morris.RData"))
load(paste0(getwd(),"/dim.RData"))

res.fact <- function(d){
  d$algorithm <- factor(d$algorithm, levels = c("P", "Pc", "PB", "PBc", "RPf", "RPfp", "RPcf", "RPcfp", 
                                                "RPx", "RPxp", "RPcx", "RPcxp", "RPs", "RPsp", "RPcs", "RPcsp",
                                                "BI", "BIc", "BI5", "RBIcfp", "RBIcxp"), ordered = TRUE)
  d
}

res <- res.fact(res)
res.f <- res.fact(res.f)
peeling.dsgc <- res.fact(peeling.dsgc)
peeling.morris <- res.fact(peeling.morris)



#### auxiliary function to compute relative improvement

get.ratios <- function(d, np, bsln = "P"){
  d <- d[npts == np & (is.na(ngen) | ngen == 10000 | ngen == 100000),]
  
  for(i in unique(d$dgp)){
    for(j in unique(d$algorithm)){
      if(j != bsln){
        d[algorithm == j & dgp == i, m.wracc := -1 + d[algorithm == j & dgp == i, m.wracc]/d[algorithm == bsln & dgp == i, m.wracc]]
        d[algorithm == j & dgp == i, m.interp := -1 + d[algorithm == j & dgp == i, m.interp]/d[algorithm == bsln & dgp == i, m.interp]]
        d[algorithm == j & dgp == i, m.consist := -1 + d[algorithm == j & dgp == i, m.consist]/d[algorithm == bsln & dgp == i, m.consist]]
        d[algorithm == j & dgp == i, m.auc := -1 + d[algorithm == j & dgp == i, m.auc]/d[algorithm == bsln & dgp == i, m.auc]]
        d[algorithm == j & dgp == i, m.prec := -1 + d[algorithm == j & dgp == i, m.prec]/d[algorithm == bsln & dgp == i, m.prec]]
      }
    }
  }
  
  d <- d[algorithm != bsln, ]
  d
}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



#########################
#####           #########
#####   PLOTS   #########
#####           #########
#########################

#### Figure 4, Demonstration. Evaluation of BI


plot.boxes.dgp <- function(d, dg = "morris"){
  
  d <- d[dgp == dg & (is.na(ngen) | ngen == 10000) & npts == 400 & algorithm %in% c("BI","BIc"),]
  d <- d[, c("algorithm", "wracct", "wracc")]
  d <- data.table(melt(d))
  d[variable == "wracct", algorithm := paste0("t", algorithm)]
  colnames(d) <- c("variable", "sh", "value")
  d$value = d$value*100
  
  cols <- rep(brewer.pal(3, "Paired")[1:2], 2)
  bs = 9
  
  p <- ggplot(d, aes(x = variable, y = value, col = variable)) +
    geom_quasirandom(size = 0.8, varwidth = TRUE, shape = d$sh) +
    scale_color_manual("", values = cols) +
    geom_boxplot(lwd = 0.2, outlier.shape = NA, color = "black", alpha = 0.5, coef = 0) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = bs - 1),
          plot.margin = unit(c(0.05,0,0.01,0.01), "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    ylab("WRAcc, %")
  
  p
}

p <- plot.boxes.dgp(res.f[(distr == "laths" | is.na(distr))], dg = "morris")
ggsave(paste0(getwd(), "/plots_tables/Fig_demo.pdf"), 
       plot = p, device = cairo_pdf, width = 2.1, height = 0.84)




#### Figures 5--6. Relative quality change in %

plot.dgpwize <- function(d, np, coef = 100, lbl = "auc"){
  
  d <- d[npts == np & (is.na(ngen) | ngen == 100000 | ngen == 10000),]
  if(lbl == "auc"){
    lbl <- "PR AUC"
    d <- d[, .(algorithm, m.auc)]
  } else if(lbl == "prec"){
    lbl <- "precision"
    d <- d[, .(algorithm, m.prec)]
  } else if(lbl == "restr"){
    lbl <- "# restricted"
    d <- d[, .(algorithm, m.interp)]
  } else if(lbl == "cons"){
    lbl <- "consistency"
    d <- d[, .(algorithm, m.consist)]
  } else if(lbl == "WRAcc"){
    lbl <- "WRAcc"
    d <- d[, .(algorithm, m.wracc)]
  } else {
    stop(paste0("undefined label ", lbl))
  }
  d[, 2] <- d[, 2]*coef
  names(d) <- c("variable", "value")
  
  d$sh  <- rep(1, nrow(d))
  d[variable == "P" | variable == "Pc" | variable == "BI" | variable == "BIc", sh := 16]
  d[variable == "PB" |variable == "PBc" | variable == "BI5", sh := 15]
  d[variable == "RPf" |variable == "RPfp" | variable == "RPcf" | variable == "RPcfp" | variable == "RBIcfp", sh := 17]
  d[variable == "RPx" | variable == "RPxp" | variable == "RPcx" | variable == "RPcxp" | variable == "RBIcxp", sh := 18]
  d[variable == "RPs" | variable == "RPsp" | variable == "RPcs" | variable == "RPcsp", sh := 6]
  
  cols <- brewer.pal(9, "Paired")
  if(length(unique(d$variable)) == 2 & sum(grepl("BI", d$variable)) > 0){
    cols <- cols[c(1,8)]
  } else if(length(unique(d$variable)) == 2 & sum(grepl("PBc", d$variable)) == 0){
    cols <- cols[c(2,8)]
  } else if(length(unique(d$variable)) == 2 & sum(grepl("RPcxp", d$variable)) > 0){
    cols <- cols[c(4,8)]
  } else if(length(unique(d$variable)) == 2 & sum(grepl("RPx", d$variable)) > 0){
    cols <- cols[c(4,7)]
  } else {
    cols <- cols[c(1,3:5,7,9)]
  }
  bs = 9
  
  denom = 50
  minus = 0
  steptck = 10
  dev = 1
  if(max(abs(d$value)) > 150) {minus = 50; steptck = 25; dev = 2} # specifically for restricted dimensions with PRIM
  brks = seq(round(min(d$value)/denom, 0)*denom, round(max(d$value)/denom)*denom, steptck)
  hl <- seq(round(min(d$value)/denom, 0)*denom, round(max(d$value)/denom)*denom, denom)
  hl <- hl[hl < 301]
  brks <- brks[brks < 301]
  if(length(brks) == 1){
    brks = seq(-50, 50, 10)
    hl = c(-50,0,50)
  }
  lbls = brks
  lbls[lbls %% (denom*dev) != 0 & abs(lbls) != 50] <- ""
  
  brks <- sign(brks)*sqrt(abs(brks))
  hl <-  sign(hl)*sqrt(abs(hl))
  d$value = sign(d$value)*sqrt(abs(d$value))

  
  p <- ggplot(d, aes(x = variable, y = value, col = variable)) +
    geom_hline(yintercept = hl, colour = "grey90", size = 0.2) +
    geom_quasirandom(size = 0.8, varwidth = TRUE, shape = d$sh) +
    scale_color_manual("", values = cols) +
    geom_boxplot(lwd = 0.2, outlier.shape = NA, color = "black", alpha = 0.5, coef = 0) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = bs - 1),
          panel.grid.major = element_blank(),
          plot.margin = unit(c(0.05,0,0.01,0.01), "cm"),
          panel.grid.minor = element_blank()) +
    ylab(lbl)
  
  p = p + scale_y_continuous(breaks = brks, labels = lbls)
  
  p
}

d <- get.ratios(res[algorithm %in% c("P", "Pc", "PB", "PBc", "RPf", "RPx", "RPs") & (distr == "laths" | is.na(distr)),], 
                np = 400, bsln = "Pc")

for(lbl in c("auc", "prec", "restr", "cons")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_P_", lbl, ".pdf"), 
         plot = p, device = cairo_pdf, width = 2.7, height = 1)
}

d <- get.ratios(res[algorithm %in% c("BI", "BIc", "RBIcxp") & (distr == "laths" | is.na(distr)),], 
                np = 400, bsln = "BIc")

for(lbl in c("WRAcc", "cons")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_BI_", lbl, ".pdf"), 
         plot = p, device = cairo_pdf, width = 0.95, height = 0.75)
}
p <- plot.dgpwize(d, np = 400, coef = 100, lbl = "restr")
ggsave(paste0(getwd(), "/plots_tables/Fig_BI_", "restr", ".pdf"),
       plot = p, device = cairo_pdf, width = 1.05, height = 0.75)




#### discr

d <- get.ratios(res[algorithm %in% c("Pc", "PBc", "RPcxp") & distr == "discr",], 
                np = 400, bsln = "PBc")

for(lbl in c("auc", "prec")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_P_", lbl, "_discr.pdf"), 
         plot = p, device = cairo_pdf, width = 0.9, height = 0.7)
}

d <- get.ratios(res[algorithm %in% c("BI", "BIc", "RBIcxp") & distr == "discr",], 
                np = 400, bsln = "BIc")

for(lbl in c("WRAcc")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_BI_", lbl, "_discr.pdf"), 
         plot = p, device = cairo_pdf, width = 0.9, height = 0.7)
}




#### logitnorm

d <- get.ratios(res[algorithm %in% c("Pc", "PBc", "RPx") & distr == "logitnorm",], 
                np = 400, bsln = "Pc")

for(lbl in c("auc", "prec")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_P_", lbl, "_ssl.pdf"), 
         plot = p, device = cairo_pdf, width = 0.9, height = 0.7)
}

d <- get.ratios(res[algorithm %in% c("BI", "BIc", "RBIcxp") & distr == "logitnorm",], 
                np = 400, bsln = "BIc")

for(lbl in c("WRAcc")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_BI_", lbl, "_ssl.pdf"), 
         plot = p, device = cairo_pdf, width = 0.9, height = 0.7)
}








############ runtimes


plot.rt <- function(d, yl = TRUE){
  
  d <- d[(is.na(ngen) | ngen == 100000 | ngen == 10000), .(npts, algorithm, m.time)]
  
  names(d) <- c("npts", "algorithm", "value")
  
  cols <- brewer.pal(9, "Paired")
  if(grepl("P", d$algorithm[1])){
    cols <- cols[c(2, 4, 5, 7)]
  } else {
    cols <- cols[c(1, 2, 8)]
  }
  bs = 9
  
  d$npts <- as.character(d$npts)
  p <- ggplot(d, aes(x = npts, y = value, fill = algorithm)) + 
    geom_boxplot(outlier.size = 0.2, lwd = 0.1) +
    scale_fill_manual("", values = cols) + 
    theme_light(base_size = bs) +
    theme(legend.position = c(0.5, 1.1),
          legend.text = element_text(size = bs - 1),
          legend.key.size = unit(0.2, 'cm'),
          legend.spacing.x = unit(0.1, 'cm'),
          legend.background = element_rect(fill = NA),
          axis.title.x = element_text(size = bs - 1),
          axis.title.y = element_text(size = bs - 1),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.margin = unit(c(0.28,0,0,0), "cm")) +
    guides(fill = guide_legend(direction = "horizontal")) +
    xlab("N")
  
  if(yl) p = p + ylab("runtime, s")
  else   p = p + theme(axis.title.y = element_blank())
  
  p
}


d <- res[algorithm %in% c("PBc", "Pc", "RPx", "RPf") 
         & npts <= 800 & (is.na(ngen) | ngen == 100000) 
         & (distr == "laths" | is.na(distr)),]
p <- plot.rt(d)

ggsave(paste0(getwd(), "/plots_tables/runtime_P_lath.pdf"), 
       plot = p, device = cairo_pdf, width = 1.75, height = 1)


d <- res[algorithm %in% c("BI", "BIc", "RBIcxp") 
         & npts <= 800 & (is.na(ngen) | ngen == 10000) 
         & (distr == "laths" | is.na(distr)),]
p <- plot.rt(d, yl = FALSE)

ggsave(paste0(getwd(), "/plots_tables/runtime_BI_lath.pdf"), 
       plot = p, device = cairo_pdf, width = 1.45, height = 1)






#### Figure 7. Peeling trajectories

plot.curves <- function(d, f = 0.4, coly = "black"){
  
  for(i in unique(d$algorithm)){
    tmp <- lowess(d[algorithm == i, 1:2], f = f)
    d[algorithm == i, 1] <- tmp[[1]]
    d[algorithm == i, 2] <- tmp[[2]]
  }
  
  cols <- brewer.pal(9, "Paired")
  cols <- cols[c(1,2,7)]
  bs = 9
  
  p <- ggplot(d, aes(x = recall, y = precision, col = algorithm, linetype = algorithm)) +
    geom_line(size = 0.5) +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 2, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = c(0.5, 1.15),
          legend.text = element_text(size = bs - 1),
          legend.spacing.x = unit(0.1, 'cm'),
          legend.key.size = unit(0.35, 'cm'),
          legend.background = element_rect(fill = NA),
          legend.title = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          axis.title = element_text(size = bs - 1),
          axis.title.y = element_text(color = coly),
          plot.margin = unit(c(0.22,0.08,0,0), "cm"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = bs - 1)) +
    guides(col = guide_legend(nrow = 1))
  
  p
}


peeling <- peeling.morris[algorithm %in% c("P", "Pc", "RPx")]
p <- plot.curves(peeling)

ggsave(paste0(getwd(), "/plots_tables/Fig_peeling_morris.pdf"), 
       plot = p, device = cairo_pdf, width = 1.5, height = 0.9)





#### Figure 8. Quality metrics

plot.boxes <- function(d, np = 400, lbl, coef = 100){
  
  d <- d[npts == np & (is.na(ngen) | ngen == 100000 | ngen == 10000) & dgp == "morris",]
  if(lbl == "auc"){
    lbl <- "PR AUC"
    d <- d[, .(algorithm, auc)]
  } else if(lbl == "prec"){
    lbl <- "precision"
    d <- d[, .(algorithm, prec)]
  } else if(lbl == "restr"){
    lbl <- "# restricted"
    d <- d[, .(algorithm, interp)]
  } else if(lbl == "WRAcc"){
    lbl <- "WRAcc"
    d <- d[, .(algorithm, wracc)]
  } else {
    stop(paste0("undefined label ", lbl))
  }
  d[, 2] <- d[, 2]*coef
  names(d) <- c("variable", "value")
  
  d$sh  <- rep(1, nrow(d))
  d[variable == "P" | variable == "Pc" | variable == "BI" | variable == "BIc", sh := 16]
  d[variable == "PB" |variable == "PBc" | variable == "BI5", sh := 15]
  d[variable == "RPf" |variable == "RPfp" | variable == "RPcf" | variable == "RPcfp" | variable == "RBIcfp", sh := 17]
  d[variable == "RPx" | variable == "RPxp" | variable == "RPcx" | variable == "RPcxp" | variable == "RBIcxp", sh := 18]
  d[variable == "RPs" | variable == "RPsp" | variable == "RPcs" | variable == "RPcsp", sh := 6]
  
  
  cols <- brewer.pal(9, "Paired")
  cols <- cols[c(1, 2, 7)]
  bs = 9
  
  p <- ggplot(d, aes(x = variable, y = value, col = variable)) +
    geom_quasirandom(size = 0.8, varwidth = TRUE, shape = d$sh) +
    scale_color_manual("", values = cols) +
    geom_boxplot(lwd = 0.2, outlier.shape = NA, color = "black", alpha = 0.5, coef = 0) +
    theme_light(base_size = bs) +
    theme(legend.position = "none",
          axis.title = element_text(size = bs - 1),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.08,0,0,0), "cm")) +
    guides(fill = guide_legend(direction = "horizontal")) +
    ylab(lbl)
  
  p
}


lbl = "auc"
p <- plot.boxes(res.f[algorithm %in% c("P", "Pc", "RPx") 
                      & (distr == "laths" | is.na(distr)),], lbl = lbl)
ggsave(paste0(getwd(), "/plots_tables/Fig_morris_", lbl, ".pdf"), 
       plot = p, device = cairo_pdf, width = 1.5, height = 0.9)


lbl = "WRAcc"
p <- plot.boxes(res.f[algorithm %in% c("BI", "BIc", "RBIcxp") 
                      & (distr == "laths" | is.na(distr)),], lbl = lbl)
ggsave(paste0(getwd(), "/plots_tables/Fig_morris_", lbl, ".pdf"), 
       plot = p, device = cairo_pdf, width = 1.5, height = 0.9)


tmp <- res.f[algorithm %in% c("Pc", "RPx") & npts == 400 & 
        (is.na(ngen) | ngen == 100000 | ngen == 10000) & dgp == "morris"]
wilcox.test(auc ~ algorithm, data = tmp)



#### Figure 9-10. Learning curves; number of generated points.


plot.np <- function(d, lbl, fn, err = TRUE, nm = fn){
  d <- d[dgp == fn & (is.na(ngen) | ngen == 100000 | ngen == 10000),]
  
  if(lbl == "auc"){
    lbl <- "PR AUC"
    d <- d[, .(npts, algorithm, auc)]
  } else if(lbl == "prec"){
    lbl <- "precision"
    d <- d[, .(npts, algorithm, prec)]
  } else if(lbl == "restr"){
    lbl <- "# restricted"
    d <- d[, .(npts, algorithm, interp)]
  } else if(lbl == "WRAcc"){
    lbl <- "WRAcc"
    d <- d[, .(npts, algorithm, wracc)]
  } else {
    stop(paste0("undefined label ", lbl))
  }
  names(d) <- c("npts", "algorithm", "value")
  d$value = d$value*100
  
  cols <- brewer.pal(9, "Paired")
  if(grepl("BI", d$algorithm[1])){
    cols <- cols[c(1, 2, 7)]
    ltys <- c(1, 1, 3)
  } else {
    cols <- cols[c(1, 2, 7, 8)]
    ltys <- c(1, 1, 3, 3)
  }
  
  preproc2 <- function(x){
    x[variable == "BI" & (npts %in% c(400, 1600)), value := NA]
    x[variable == "BI5" & (npts %in% c(400, 1600)), value := NA]
    x[variable == "BIc" & (npts %in% c(200, 800, 3200)), value := NA]
    x[variable == "P" & (npts %in% c(200, 800, 3200)), value := NA]
    x[variable == "PB" & (npts %in% c(200, 800, 3200)), value := NA]
    x[variable == "Pc" & (npts %in% c(400, 1600)), value := NA]
    x[variable == "PBc" & (npts %in% c(200, 800, 3200)), value := NA]
    x[variable == "PP" & (npts %in% c(200, 800, 3200)), value := NA]
    x[variable == "RPx" & (npts %in% c(200, 800, 3200)), value := NA]
    x[variable == "RPxp" & (npts %in% c(400, 1600)), value := NA]
    x
  }
  
  tmp1 <- data.table(melt(cast(d, npts~algorithm, median, value = "value"), id = "npts"))
  tmp2 <- data.table(melt(cast(d, npts~algorithm, function(x) quantile(x, 0.25), value = "value"), id = "npts"))
  tmp2 <- preproc2(tmp2)
  names(tmp2)[names(tmp2) == "value"] = "low"
  tmp3 <- data.table(melt(cast(d, npts~algorithm, function(x) quantile(x, 0.75), value = "value"), id = "npts"))
  tmp3 <- preproc2(tmp3)
  names(tmp3)[names(tmp3) == "value"] = "high"
  d <- merge(merge(tmp1, tmp2), tmp3)
  
  bs = 9
  npts = sort(unique(d$npts))
  
  p <- ggplot(d, aes(x = npts, y = value, col = variable, linetype = variable)) +
    geom_line(size = 0.5) +
    scale_color_manual("", values = cols) +
    scale_x_continuous(breaks = npts, labels = npts/1000, trans = "log2") +
    scale_linetype_manual("", values = ltys) +
    theme_light(base_size = bs) +
    theme(
      legend.position = c(0.5, 0.9),
      legend.title = element_blank(),
      legend.text = element_text(size = bs - 1),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA),
      legend.spacing.x = unit(0.1, 'cm'),
      axis.title = element_text(size = bs - 1),
      axis.text.x = element_text(angle = 90),
      plot.margin = unit(c(0,0.12,0,0), "cm"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = bs - 1)) +
    guides(col = guide_legend(nrow = 1)) +
    ggtitle(nm) + 
    ylab(lbl) +
    xlab("N, thousands")
  
  if(err) p = p + geom_errorbar(aes(ymin = low, ymax = high), linetype = 1, width = 0.2, lwd = 0.3)
  
  p
}



plot.ngen <- function(d, lbl, fn, np = 400, err = TRUE, nm = fn){
  d <- d[dgp == fn & npts == np & (is.na(d$ngen) | d$ngen != 100000),]
  d[is.na(d$ngen), ngen := 200]
  ng <- unique(d$ngen)
  ngl <- ng/1000
  
  if(lbl == "auc"){
    lbl <- "PR AUC"
    d <- d[, .(ngen, algorithm, auc)]
  } else if(lbl == "prec"){
    lbl <- "precision"
    d <- d[, .(ngen, algorithm, prec)]
  } else if(lbl == "restr"){
    lbl <- "# restricted"
    d <- d[, .(ngen, algorithm, interp)]
  } else if(lbl == "WRAcc"){
    lbl <- "WRAcc"
    d <- d[, .(ngen, algorithm, wracc)]
  } else {
    stop(paste0("undefined label ", lbl))
  }
  names(d) <- c("ngen", "algorithm", "value")
  d$value = d$value*100
  
  
  cols <- brewer.pal(9, "Paired")
  if(grepl("BI", d$algorithm[1])){
    cols <- cols[c(1, 2, 7)]
    ltys <- c(1, 1, 3)
  } else {
    cols <- cols[c(1, 2, 7, 8)]
    ltys <- c(1, 1, 3, 3)
  }
  bs = 9
  
  preproc <- function(x){
    x[, BI := x$BI[1]]
    x[, BI5 := x$BI5[1]]
    x[, BIc := x$BIc[1]]
    x[, P := x$P[1]]
    x[, PP := x$PP[1]]
    x[, Pc := x$Pc[1]]
    x[, PB := x$PB[1]]
    x[, PBc := x$PBc[1]]
    x <- melt(x, id = "ngen")
  }
  
  preproc2 <- function(x){
    x[variable == "BI" & ngen != 10000, value := NA]
    x[variable == "BI5" & ngen != 3200, value := NA]
    x[variable == "BIc" & ngen != 6400, value := NA]
    x[variable == "P" & ngen != 25000, value := NA]
    x[variable == "PB" & ngen != 3200, value := NA]
    x[variable == "Pc" & ngen != 6400, value := NA]
    x[variable == "PBc" & ngen != 1600, value := NA]
    x[variable == "PP" & ngen != 800, value := NA]
    x[variable == "RPx" & !(ngen %in% c(200, 800, 3200, 25000)), value := NA]
    x[variable == "RPxp" & !(ngen %in% c(400, 1600, 6400, 100000)), value := NA]
    x
  }
  
  tmp1 <- data.table(preproc(data.table(cast(d, ngen~algorithm, median, value = "value"))))
  tmp2 <- data.table(preproc(data.table(cast(d, ngen~algorithm, function(x) quantile(x, 0.25), value = "value"))))
  tmp2 <- preproc2(tmp2)
  names(tmp2)[names(tmp2) == "value"] = "low"
  tmp3 <- data.table(preproc(data.table(cast(d, ngen~algorithm, function(x) quantile(x, 0.75), value = "value"))))
  tmp3 <- preproc2(tmp3)
  names(tmp3)[names(tmp3) == "value"] = "high"
  d <- merge(merge(tmp1, tmp2), tmp3)
  
  p <- ggplot(d, aes(x = ngen, y = value, col = variable, linetype = variable)) +
    geom_line(size = 0.5) +
    scale_color_manual("", values = cols) +
    scale_x_continuous(breaks = ng, labels = ngl, trans = "log2") +
    scale_linetype_manual("", values = ltys) +
    theme_light(base_size = bs) +
    theme(
      legend.position = c(0.5, 0.9),
      legend.title = element_blank(),
      legend.text = element_text(size = bs - 1),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA),
      legend.spacing.x = unit(0.1, 'cm'),
      axis.title = element_text(size = bs - 1),
      axis.text.x = element_text(angle = 90),
      plot.margin = unit(c(0,0.12,0,0), "cm"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = bs - 1)) +
    guides(col = guide_legend(nrow = 1)) +
    ggtitle(nm) + 
    ylab(lbl) +
    xlab("L, thousands")
  
  if(err) p = p + geom_errorbar(aes(ymin = low, ymax = high), linetype = 1, width = 0.3, lwd = 0.3)
  
  p
}

d <- res.f[algorithm %in% c("P", "Pc", "RPx", "RPxp") & (distr == "laths" | is.na(distr)),]
p1 <- plot.np(d, fn = "morris", lbl = "auc", nm = "morris, L=100000")
p2 <- plot.ngen(d, fn = "morris", lbl = "auc", nm = "morris, N=400")
mylegend <- g_legend(plot.np(d, fn = "morris", lbl = "auc", err = FALSE))
p3 <- grid.arrange(mylegend,
                   arrangeGrob(p1 + theme(legend.position = "none"),
                               ggplot() + theme_void() + xlab(NULL),
                               p2 + theme(legend.position = "none"),
                               nrow = 1, widths = c(10,1,10)), nrow = 2, heights = c(1, 7))
ggsave(paste0(getwd(), "/plots_tables/Fig_P_np_ngen.pdf"), 
       plot = p3, device = cairo_pdf, width = 3.2, height = 1.3)



d <- res.f[algorithm %in% c("BI", "BIc", "RBIcxp") & (distr == "laths" | is.na(distr)),]
p1 <- plot.np(d, fn = "morris", lbl = "WRAcc", nm = "morris, L=10000")
p2 <- plot.ngen(d, fn = "morris", lbl = "WRAcc", nm = "morris, N=400")
mylegend <- g_legend(plot.np(d, fn = "morris", lbl = "WRAcc", err = FALSE))
p3 <- grid.arrange(mylegend,
                   arrangeGrob(p1 + theme(legend.position = "none"),
                               ggplot() + theme_void() + xlab(NULL),
                               p2 + theme(legend.position = "none"),
                               nrow = 1, widths = c(10,1,10)), nrow = 2, heights = c(1, 7))
ggsave(paste0(getwd(), "/plots_tables/Fig_BI_np_ngen.pdf"), 
       plot = p3, device = cairo_pdf, width = 3.2, height = 1.3)






#########################
#####           #########
#####  TABLES   #########
#####           #########
#########################

get.mask <- function(d, best = "max", np = c(200, 400, 800), value = "auc",
                     coef = 100, rn = 1, addvol = FALSE, add.dim = FALSE, d.dim = NULL, nm = "", csv = TRUE){
  
  nm.add <- ifelse(sum(grepl("P", d$algorithm)), "P", "BI")
  d <- d[npts %in% np & (is.na(ngen) | ngen == 100000 | ngen == 10000),]
  d <- d[is.na(dgp), dgp := "dsgc"]
  tmp <- cast(d, dgp ~ algorithm ~ npts, value = paste0("m.", value))*coef
  tabs <- stabs <- list()
  
  for(j in 1:length(np)){
    
    mask <- dt <- tmp[, , j]
    tabs[[j]] <- round(dt, rn)
    if(rn >= 2){
      tabs[[j]][dt > 10] <- round(dt[dt > 10], rn - 1)
    }
    mask[,] <- 0
    b0 <- round(apply(dt, 2, mean), rn)
    
    if(best == "min"){
      dt <- -dt
    }
    
    inds <- apply(dt, 1, function(x) which(x == max(x)))
    for(i in 1:nrow(mask)){
      mask[i, inds[[i]]] <- 1
      dt[i, inds[[i]]] <- -100
      tabs[[j]][i, inds[[i]]] <- paste0("\\textbf{", tabs[[j]][i, inds[[i]]], "}")
    }
    inds <- apply(dt, 1, function(x) which(x == max(x)))
    for(i in 1:nrow(mask)){
      if(sum(mask[i,]) < 2){
        mask[i, inds[[i]]] <- 2
        tabs[[j]][i, inds[[i]]] <- paste0("\\textit{", tabs[[j]][i, inds[[i]]], "}")
      }
    }
    
    b1 <- apply(mask, 2, function(x) sum(x == 1))
    b2 <- apply(mask, 2, function(x) sum(x == 2))
    tabs[[j]] <- rbind(tabs[[j]], b0, b1, b2)
    n <- length(row.names(tabs[[j]]))
    row.names(tabs[[j]])[(n - 2):n] <- c("avg", "\\# 1","\\# 2")
    
    if(addvol){
      dt <- as.numeric(cast(d[npts == np[j],], npts~algorithm, mean, value = "m.boxvol")[, -1])
      dt <- round(dt*coef, rn)
      tabs[[j]] <- rbind(tabs[[j]], dt)
      row.names(tabs[[j]])[length(row.names(tabs[[j]]))] <- "avg box vol."
    }
    
    stabs[[j]] <- matrix(c(np[j], b0), nrow = 1)
    colnames(stabs[[j]]) <- c("npts", colnames(mask))
  }
  
  res.excel <- tmp[,, 1]
  for(j in 2:length(np)){
    tabs[[1]] <- cbind(tabs[[1]], tabs[[j]])
    stabs[[1]] <- rbind(stabs[[1]], stabs[[j]])
    res.excel <- cbind(res.excel, tmp[,, j]) 
  }
  stabs <- stabs[[1]]
  
  if(addvol){
    res.excel <- rbind(res.excel, tabs[[1]][nrow(tabs[[1]]),])
    row.names(res.excel)[length(row.names(res.excel))] <- "avg box vol."
  }
  if(csv)  write.csv(res.excel, paste0(getwd(), "/plots_tables/Tab_", nm.add, "_", value, nm, ".csv"))
  
  if(add.dim){
    n = nrow(tabs[[1]]) - nrow(d.dim)
    tmp <- rbindlist(list(d.dim[, 2:3], data.table(dim = rep(" ", n), inf.dim = "")))
    colnames(tmp) <- c("D", "I")
    rnames <- row.names(tabs[[1]])
    tabs[[1]] <- cbind(tabs[[1]], tmp)
    row.names(tabs[[1]]) <- rnames
  }
  
  basb <- function(dt, best){
    mlt <- 1
    if(best == "min"){
      mlt <- -1
    }
    dtmp <- dt
    for(i in 1:nrow(dt)){
      tmp <- dt[i,]*mlt
      dtmp[i, which(tmp == max(tmp))] <- paste0("\\textbf{", max(tmp)*mlt, "}")
      tmp[which(tmp == max(tmp))] = -Inf
      dtmp[i, which(tmp == max(tmp))] <- paste0("\\textit{", max(tmp)*mlt, "}")
    }
    dtmp
  }
  stabs[, 2:ncol(stabs)] <- basb(stabs[, 2:ncol(stabs)], best)
  stabs <- rbind(stabs, c("$\\textrm{mor}_{800}$", tabs[[3]][rownames(tabs[[3]]) == "morris", ]))
  colnames(stabs)[colnames(stabs) == "npts"] <- "N"
  
  write.table(stabs, paste0(getwd(), "/plots_tables/Tabs_", nm.add, "_", value, nm, ".txt"), 
              quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)
}

get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("P", "Pc", "PB", "PBc", "RPf", "RPx", "RPs"),], 
         best = "max", value = "auc",  coef = 100, rn = 1, addvol = FALSE, csv = FALSE)
get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("P", "Pc", "PB", "PBc", "RPf", "RPx", "RPs"),], 
         best = "max", value = "prec", coef = 100, rn = 1, addvol = FALSE, csv = FALSE)
get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("P", "Pc", "PB", "PBc", "RPf", "RPx", "RPs"),], 
         best = "min", value = "interp", coef = 1, rn = 2, addvol = FALSE, add.dim = TRUE, d.dim = d.dim, csv = FALSE)
get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("P", "Pc", "PB", "PBc", "RPf", "RPx", "RPs"),], 
         best = "max", value = "consist", coef = 100, rn = 1, addvol = FALSE, csv = FALSE)

get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("BI", "BIc", "BI5", "RBIcfp", "RBIcxp"),], 
         best = "max", value = "wracc", coef = 100, rn = 1, addvol = FALSE, csv = FALSE)
get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("BI", "BIc", "BI5", "RBIcfp", "RBIcxp"),],  
         best = "min", value = "interp", coef = 1, rn = 2, addvol = FALSE, csv = FALSE, add.dim = TRUE, d.dim = d.dim)
get.mask(res[(distr == "laths" | is.na(distr)) & algorithm %in% c("BI", "BIc", "BI5", "RBIcfp", "RBIcxp"),], 
         best = "max", value = "consist", coef = 100, rn = 1, addvol = FALSE, csv = FALSE)



#### irrelevant dimensions

d <- d.dim[dim > inf.dim,]
tmp <- res[npts %in% c(200, 400, 800) & (is.na(ngen) | ngen == 100000) & 
             algorithm %in% c("P", "Pc", "PB", "PBc", "RPf", "RPx", "RPs") & (distr == "laths" | is.na(distr)),]
d <- ijoin(d, tmp, by = "dgp")
d[, dim.r := m.interp - inf.dim]
d[dim.r < 0, dim.r := 0]

d <- d[, .(restricted = mean(dim.r)), by = c("npts", "algorithm")]
d <- round(cast(d, npts ~ algorithm, value = "restricted"), 2)
colnames(d)[1] <- "\\lvert d\\rvert"
write.table(d, paste0(getwd(), "/plots_tables/Tabs_P_irrel.txt"), 
            quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)


d <- d.dim[dim > inf.dim,]
tmp <- res[npts %in% c(200, 400, 800) & (is.na(ngen) | ngen == 100000) & 
             algorithm %in% c("BI", "BIc", "BI5", "RBIcfp", "RBIcxp") & (distr == "laths" | is.na(distr)),]
d <- ijoin(d, tmp, by = "dgp")
d[, dim.r := m.interp - inf.dim]
d[dim.r < 0, dim.r := 0]

d <- d[, .(restricted = mean(dim.r)), by = c("npts", "algorithm")]
d <- round(cast(d, npts ~ algorithm, value = "restricted"), 2)
colnames(d)[1] <- "\\lvert d\\rvert"
write.table(d, paste0(getwd(), "/plots_tables/Tabs_BI_irrel.txt"), 
            quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)




# #### stats peel alpha (paragraph 7.1)
# 
# mean(res[grepl("Pc", algorithm) & npts == 200, m.alpha])
# mean(res[grepl("Pc", algorithm) & npts == 800, m.alpha])
# for(i in c(200, 400, 800, 1600, 3200)){
#   print(mean(res.f[grepl("Pc", algorithm) & npts == i & dgp == "morris", alpha]))
# }





