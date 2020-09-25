
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


#########################
#####           #########
#####   PLOTS   #########
#####           #########
#########################

#### Figure 4, Demonstration. Evaluation of BI


plot.boxes.dgp <- function(d, dg = "morris"){
  
  d <- d[dgp == dg & (is.na(ngen) | ngen == 10000) & npts == 400 & algorithm %in% c("BI","BIc"),]
  d <- d[, c("algorithm", "wracct", "wracc")]
  d <- melt(d)
  d[variable == "wracct", algorithm := paste0("t", algorithm)]
  colnames(d) <- c("variable", "sh", "value")
  d$value = d$value*100
  
  cols <- brewer.pal(4, "Paired")
  bs = 9
  
  p <- ggplot(d, aes(x = variable, y = value, col = variable)) +
    geom_quasirandom(size = 0.8, varwidth = TRUE, shape = d$sh) +
    scale_color_manual("", values = cols) +
    geom_boxplot(lwd = 0.2, outlier.shape = NA, color = "black", alpha = 0.5, coef = 0) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = bs - 2),
          plot.margin = unit(c(0.05,0,0.01,0.01), "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) +
    ylab("WRAcc, %")
  
  p
}

p <- plot.boxes.dgp(res.f, dg = "morris")
ggsave(paste0(getwd(), "/plots_tables/Fig_demo.pdf"), 
       plot = p, device = cairo_pdf, width = 2, height = 0.8)


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
  d[variable == "P" | variable == "PP" | variable == "Pc" | variable == "BI" | variable == "BIc", sh := 16]
  d[variable == "PB" |variable == "PBc" | variable == "BI5", sh := 15]
  d[variable == "REDSl" |variable == "REDSp" | variable == "REDS", sh := 17]
  
  d$variable <- factor(d$variable, levels = c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp", 
                                              "BI", "BIc", "BI5", "REDS"), ordered = TRUE)
  
  cols <- brewer.pal(6, "Paired")
  if(length(unique(d$variable)) == 2){
    cols <- cols[c(1, 6)]
  }
  bs = 8
  
  denom = 50
  minus = 0
  steptck = 10
  dev = 1
  if(max(d$value) > 190) {minus = 50; steptck = 25; dev = 2} # specifically for restricted dimensions with PRIM
  brks = seq(round(min(d$value)/denom, 0)*denom, ceiling(max(d$value - minus)/denom)*denom, steptck)
  hl <- seq(round(min(d$value)/denom, 0)*denom, ceiling(max(d$value - minus)/denom)*denom, denom)
  lbls = brks
  lbls[lbls %% (denom*dev) != 0] <- ""
  
  brks <- sign(brks)*sqrt(abs(brks))
  hl <-  sign(hl)*sqrt(abs(hl))
  d$value = sign(d$value)*sqrt(abs(d$value))

  
  p <- ggplot(d, aes(x = variable, y = value, col = variable)) +
    geom_hline(yintercept = hl, colour = "lightgrey", size = 0.2) +
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

d <- get.ratios(res[algorithm %in% c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp"),], np = 400, bsln = "Pc")

for(lbl in c("auc", "prec", "restr", "cons")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_P_", lbl, ".pdf"), 
         plot = p, device = cairo_pdf, width = 2.5, height = 0.9)
}

d <- get.ratios(res[algorithm %in% c("BI", "BIc", "REDS"),], np = 400, bsln = "BIc")

for(lbl in c("WRAcc", "cons")){
  p <- plot.dgpwize(d, np = 400, coef = 100, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/Fig_BI_", lbl, ".pdf"), 
         plot = p, device = cairo_pdf, width = 0.9, height = 0.7)
}
p <- plot.dgpwize(d, np = 400, coef = 100, lbl = "restr")
ggsave(paste0(getwd(), "/plots_tables/Fig_BI_", "restr", ".pdf"), 
       plot = p, device = cairo_pdf, width = 0.99, height = 0.7)




#### Figure 7. Peeling trajectories

plot.curves <- function(d, f = 0.4, label = "", coly = "black"){
  
  for(i in unique(d$algorithm)){
    tmp <- lowess(d[algorithm == i, 1:2], f = f)
    d[algorithm == i, 1] <- tmp[[1]]
    d[algorithm == i, 2] <- tmp[[2]]
  }
  
  cols <- brewer.pal(6, "Paired")
  bs = 9
  
  d$algorithm <- factor(d$algorithm, levels = c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp", 
                                              "BI", "BIc", "BI5", "REDS"), ordered = TRUE)
  
  p <- ggplot(d, aes(x = recall, y = precision, col = algorithm, linetype = algorithm)) +
    geom_line(size = 0.4) +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(
          legend.position = c(0.5, 0.9),
          legend.title = element_blank(),
          legend.text = element_text(size = bs - 2),
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.spacing.x = unit(0.1, 'cm'),
          axis.title = element_text(size = bs - 1),
          axis.title.y = element_text(color = coly),
          plot.margin = unit(c(0,0.12,0,0), "cm"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = bs - 1)) +
    guides(col = guide_legend(nrow = 1)) +
    ggtitle(label)
  
  p
}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


peeling <- peeling.morris[!(algorithm %in% c("PP"))]
p1 <- plot.curves(peeling, label = "morris")
peeling <- peeling.dsgc[!(algorithm %in% c("PP"))]
p2 <- plot.curves(peeling, label = "dsgc", coly = "white")
 
mylegend <- g_legend(p1)

p3 <- grid.arrange(mylegend,
                   arrangeGrob(p1 + theme(legend.position="none"),
                                p2 + theme(legend.position="none"),
                                nrow = 1), nrow = 2, heights = c(1, 7))

ggsave(paste0(getwd(), "/plots_tables/Fig_peeling.pdf"), 
       plot = p3, device = cairo_pdf, width = 3, height = 1.3)


#### Figure 8. Quality metrics

plot.boxes <- function(d, np = 400, lbl, fun, coef = 100){
  
  d <- d[npts == np & (is.na(ngen) | ngen == 100000 | ngen == 10000) & dgp == fun,]
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
  d[variable == "P" | variable == "PP" | variable == "Pc" | variable == "BI" | variable == "BIc", sh := 16]
  d[variable == "PB" |variable == "PBc" | variable == "BI5", sh := 15]
  d[variable == "REDSl" |variable == "REDSp" | variable == "REDS", sh := 17]
  
  d$variable <- factor(d$variable, levels = c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp", 
                                              "BI", "BIc", "BI5", "REDS"), ordered = TRUE)
  
  
  cols <- brewer.pal(6, "Paired")
  cols <- cols[c(1, 2, 6)]
  bs = 9
  
  p <- ggplot(d, aes(x = variable, y = value, col = variable)) +
    geom_quasirandom(size = 0.8, varwidth = TRUE, shape = d$sh) +
    scale_color_manual("", values = cols) +
    geom_boxplot(lwd = 0.2, outlier.shape = NA, color = "black", alpha = 0.5, coef = 0) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = bs - 1),
          plot.margin = unit(c(0.0,0,0.0,0.0), "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(size = bs - 1)) +
    ylab(lbl) +
    ggtitle(fun)
  
 p
}

fun = "morris"
lbl = "auc"
p <- plot.boxes(res.f[algorithm %in% c("Pc", "PBc", "REDSl"),], lbl = lbl, fun = fun)
ggsave(paste0(getwd(), "/plots_tables/Fig_", fun, "_", lbl, ".pdf"), 
       plot = p, device = cairo_pdf, width = 1.4, height = 1)

fun = "dsgc"
lbl = "auc"
p <- plot.boxes(res.f[algorithm %in% c("Pc", "PBc", "REDSl"),], lbl = lbl, fun = fun)
ggsave(paste0(getwd(), "/plots_tables/Fig_", fun, "_", lbl, ".pdf"), 
       plot = p, device = cairo_pdf, width = 1.4, height = 1)

fun = "morris"
lbl = "WRAcc"
p <- plot.boxes(res.f[algorithm %in% c("BI", "BIc", "REDS"),], lbl = lbl, fun = fun)
ggsave(paste0(getwd(), "/plots_tables/Fig_", fun, "_", lbl, ".pdf"), 
       plot = p, device = cairo_pdf, width = 1.4, height = 1)

fun = "dsgc"
lbl = "WRAcc"
p <- plot.boxes(res.f[algorithm %in% c("BI", "BIc", "REDS"),], lbl = lbl, fun = fun)
ggsave(paste0(getwd(), "/plots_tables/Fig_", fun, "_", lbl, ".pdf"), 
       plot = p, device = cairo_pdf, width = 1.4, height = 1)



#### Figure 9. Learning curves

plot.np <- function(d, fn, lbl = "auc"){
  d <- d[dgp == fn & (is.na(ngen) | ngen == 100000 | ngen == 10000),]
  
  if(lbl == "auc"){
    lbl <- "PR AUC"
    d <- d[, .(npts, algorithm, m.auc)]
  } else if(lbl == "prec"){
    lbl <- "precision"
    d <- d[, .(npts, algorithm, m.prec)]
  } else if(lbl == "restr"){
    lbl <- "# restricted"
    d <- d[, .(npts, algorithm, m.interp)]
  } else if(lbl == "cons"){
    lbl <- "consistency"
    d <- d[, .(npts, algorithm, m.consist)]
  } else if(lbl == "WRAcc"){
    lbl <- "WRAcc"
    d <- d[, .(npts, algorithm, m.wracc)]
  } else {
    stop(paste0("undefined label ", lbl))
  }
  names(d) <- c("npts", "algorithm", "value")
  
  cols <- brewer.pal(6, "Paired")
  ltys <- c(1, 1, 2, 2, 3, 3)
  if(length(unique(d$algorithm)) == 4){
    ltys <- c(1, 1, 2, 3)
    cols <- cols[c(1, 2, 4, 6)]
  } 
  bs = 9
  npts = sort(unique(d$npts))

  d$algorithm <- factor(d$algorithm, levels = c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp", 
                                                "BI", "BIc", "BI5", "REDS"), ordered = TRUE)
  
  p <- ggplot(d, aes(x = npts, y = value, col = algorithm, linetype = algorithm)) +
    geom_line(size = 0.4) +
    scale_x_continuous(breaks = npts, labels = npts, trans = "log2") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = ltys) +
    theme_light(base_size = bs) +
    theme(
      legend.position = c(0.5, 0.95),
      legend.title = element_blank(),
      legend.text = element_text(size = bs - 2),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA),
      legend.spacing.x = unit(0.1, 'cm'),
      axis.title = element_text(size = bs - 1),
      plot.margin = unit(c(0,0.14,-0.06,0), "cm"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = bs - 1)) +
    guides(col = guide_legend(nrow = 1)) +
    ylab(lbl) +
    xlab("N") +
    ggtitle(fn)
  
  p
}

plot.two <- function(d, fn1, fn2, lbl, nm){

  p1 <- plot.np(d, fn = fn1, lbl = lbl)
  p2 <- plot.np(d, fn = fn2, lbl = lbl)
  
  mylegend <- g_legend(p1)
  
  p3 <- grid.arrange(mylegend,
                     arrangeGrob(p1 + theme(legend.position = "none"),
                                 ggplot() + theme_void() + xlab(NULL),
                                 p2 + theme(legend.position = "none"),
                                 nrow = 1, widths = c(10,1,10)), nrow = 2, heights = c(1, 7))
  
  ggsave(paste0(getwd(), "/plots_tables/Fig_", nm, "_", lbl, "_lc.pdf"), 
         plot = p3, device = cairo_pdf, width = 3.2, height = 1.15)
}


d <- res[algorithm %in% c("P", "Pc", "PB", "PBc", "REDSl", "REDSp") & npts != 2000,]
plot.two(d, "dsgc", "morris", "auc", "P")

d <- res[algorithm %in% c("BI", "BIc", "BI5", "REDS") & npts != 2000,]
plot.two(d, "dsgc", "morris", "WRAcc", "BI")



#### Figure 10. number of generated points.


plot.ngen <- function(d, lbl, fn, np = 400){
  d <- d[dgp == fn & npts == np,]
  d[is.na(d$ngen), ngen := 200]
  ng <- unique(d$ngen)
  ngl <- ng/1000
  
  if(lbl == "auc"){
    lbl <- "PR AUC"
    d <- d[, .(ngen, algorithm, m.auc)]
  } else if(lbl == "prec"){
    lbl <- "precision"
    d <- d[, .(ngen, algorithm, m.prec)]
  } else if(lbl == "restr"){
    lbl <- "# restricted"
    d <- d[, .(ngen, algorithm, m.interp)]
  } else if(lbl == "cons"){
    lbl <- "consistency"
    d <- d[, .(ngen, algorithm, m.consist)]
  } else if(lbl == "WRAcc"){
    lbl <- "WRAcc"
    d <- d[, .(ngen, algorithm, m.wracc)]
  } else {
    stop(paste0("undefined label ", lbl))
  }
  names(d) <- c("ngen", "algorithm", "value")
  
  cols <- brewer.pal(6, "Paired")
  ltys <- c(1, 1, 2, 2, 3, 3)
  if(length(unique(d$algorithm)) == 4){
    ltys <- c(1, 1, 2, 3)
    cols <- cols[c(1, 2, 4, 6)]
  } 
  bs = 9
  
  d <- data.table(cast(d, ngen~algorithm, mean, value = "value"))
  d[, BI := d$BI[1]]
  d[, BI5 := d$BI5[1]]
  d[, BIc := d$BIc[1]]
  d[, P := d$P[1]]
  d[, PP := d$PP[1]]
  d[, Pc := d$Pc[1]]
  d[, PB := d$PB[1]]
  d[, PBc := d$PBc[1]]

  d <- melt(d, id = "ngen")
  
  d$algorithm <- factor(d$algorithm, levels = c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp", 
                                                "BI", "BIc", "BI5", "REDS"), ordered = TRUE)
  
  p <- ggplot(d, aes(x = ngen, y = value, col = variable, linetype = variable)) +
    geom_line(size = 0.4) +
    scale_color_manual("", values = cols) +
    scale_x_continuous(breaks = ng, labels = ngl, trans = "log2") +
    scale_linetype_manual("", values = ltys) +
    theme_light(base_size = bs) +
    theme(
      legend.position = c(0.5, 0.9),
      legend.title = element_blank(),
      legend.text = element_text(size = bs - 2),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_rect(fill = NA),
      legend.spacing.x = unit(0.1, 'cm'),
      axis.title = element_text(size = bs - 1),
      axis.text.x = element_text(angle = 90),
      plot.margin = unit(c(0,0.12,0,0), "cm"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = bs - 1)) +
    guides(col = guide_legend(nrow = 1)) +
    ggtitle(fn) + 
    ylab(lbl) +
    xlab("L, thousands")
  
p
}


p1 <- plot.ngen(res[algorithm %in% c("P", "Pc", "PB", "PBc", "REDSl", "REDSp"),], 
                  fn = "dsgc", lbl = "auc")
p2 <- plot.ngen(res[algorithm %in% c("P", "Pc", "PB", "PBc", "REDSl", "REDSp"),], 
                fn = "morris", lbl = "auc")
mylegend <- g_legend(p1)
p3 <- grid.arrange(mylegend,
                   arrangeGrob(p1 + theme(legend.position = "none"),
                               ggplot() + theme_void() + xlab(NULL),
                               p2 + theme(legend.position = "none"),
                               nrow = 1, widths = c(10,1,10)), nrow = 2, heights = c(1, 7))
ggsave(paste0(getwd(), "/plots_tables/Fig_P_ngen.pdf"), 
       plot = p3, device = cairo_pdf, width = 3.2, height = 1.20)


p1 <- plot.ngen(res[algorithm %in% c("BI", "BIc", "BI5", "REDS"),], 
                fn = "dsgc", lbl = "WRAcc")
p2 <- plot.ngen(res[algorithm %in% c("BI", "BIc", "BI5", "REDS") & (is.na(ngen) | ngen != 20000),], 
                fn = "morris", lbl = "WRAcc")
mylegend <- g_legend(p1)
p3 <- grid.arrange(mylegend,
                   arrangeGrob(p1 + theme(legend.position = "none"),
                               ggplot() + theme_void() + xlab(NULL),
                               p2 + theme(legend.position = "none"),
                               nrow = 1, widths = c(10,1,10)), nrow = 2, heights = c(1, 7))
ggsave(paste0(getwd(), "/plots_tables/Fig_BI_ngen.pdf"), 
       plot = p3, device = cairo_pdf, width = 3.2, height = 1.15)



#########################
#####           #########
#####  TABLES   #########
#####           #########
#########################

get.mask <- function(d, best = "max", np = c(200, 400, 800), value = "auc",
                     coef = 100, rn = 1, addvol = FALSE, add.dim = FALSE, d.dim = NULL){
  
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
        tabs[[j]][i, inds[[i]]] <- paste0("{\\ul ", tabs[[j]][i, inds[[i]]], "}")
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
  stabs[[1]] <- rbind(stabs[[1]], c("$\\textrm{mor}_{800}$", tabs[[j]][rownames(tabs[[j]]) == "morris", ]))
  
  if(addvol){
    res.excel <- rbind(res.excel, tabs[[1]][nrow(tabs[[1]]),])
    row.names(res.excel)[length(row.names(res.excel))] <- "avg box vol."
  }
  write.csv(res.excel, paste0(getwd(), "/plots_tables/Tab_", nm.add, "_", value, ".csv"))
  
  if(add.dim){
    n = nrow(tabs[[1]]) - nrow(d.dim)
    tmp <- rbindlist(list(d.dim[, 2:3], data.table(dim = rep(" ", n), inf.dim = "")))
    colnames(tmp) <- c("D", "I")
    rnames <- row.names(tabs[[1]])
    tabs[[1]] <- cbind(tabs[[1]], tmp)
    row.names(tabs[[1]]) <- rnames
  }
  
  write.table(stabs[[1]], paste0(getwd(), "/plots_tables/Tabs_", nm.add, "_", value, ".txt"), 
              quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)
}

get.mask(res[algorithm %in% c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp"),], best = "max", value = "auc",  coef = 100, rn = 1, addvol = FALSE)
get.mask(res[algorithm %in% c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp"),], best = "max", value = "prec", coef = 100, rn = 1, addvol = FALSE)
get.mask(res[algorithm %in% c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp"),], best = "min", value = "interp", coef = 1, rn = 2, addvol = FALSE, add.dim = TRUE, d.dim = d.dim)
get.mask(res[algorithm %in% c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp"),], best = "max", value = "consist", coef = 100, rn = 1, addvol = TRUE)

get.mask(res[algorithm %in% c("BI", "BIc", "BI5", "REDS"),], best = "max", value = "wracc", coef = 100, rn = 1, addvol = FALSE)
get.mask(res[algorithm %in% c("BI", "BIc", "BI5", "REDS"),], best = "min", value = "interp", coef = 1, rn = 2, addvol = FALSE, add.dim = TRUE, d.dim = d.dim)
get.mask(res[algorithm %in% c("BI", "BIc", "BI5", "REDS"),], best = "max", value = "consist", coef = 100, rn = 1, addvol = TRUE)



#### irrelevant dimensions

d <- d.dim[dim > inf.dim,]
d <- ijoin(d, res, by = "dgp")
d <- d[npts %in% c(200, 400, 800) & (is.na(ngen) | ngen == 100000) & algorithm %in% c("P", "Pc", "PP", "PB", "PBc", "REDSl", "REDSp"),]
d[, dim.r := m.interp - inf.dim]
d[dim.r < 0, dim.r := 0]

d <- d[, .(restricted = mean(dim.r)), by = c("npts", "algorithm")]
d <- round(cast(d, npts ~ algorithm, value = "restricted"), 2)
colnames(d)[1] <- "\\lvert d\\rvert"
write.table(d, paste0(getwd(), "/plots_tables/Tabs_P_irrel.txt"), 
            quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)


d <- d.dim[dim > inf.dim,]
d <- ijoin(d, res, by = "dgp")
d <- d[npts %in% c(200, 400, 800) & (is.na(ngen) | ngen == 10000) & algorithm %in%  c("BI", "BIc", "BI5", "REDS"),]
d[, dim.r := m.interp - inf.dim]
d[dim.r < 0, dim.r := 0]

d <- d[, .(restricted = mean(dim.r)), by = c("npts", "algorithm")]
d <- round(cast(d, npts ~ algorithm, value = "restricted"), 2)
colnames(d)[1] <- "\\lvert d\\rvert"
write.table(d, paste0(getwd(), "/plots_tables/Tabs_BI_irrel.txt"), 
            quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)




#### stats peel alpha (paragraph 7.1)

mean(res[grepl("Pc", algorithm) & npts == 200, m.alpha])
mean(res[grepl("Pc", algorithm) & npts == 800, m.alpha])
for(i in c(200, 400, 800, 1600, 3200)){
  print(mean(res.f[grepl("Pc", algorithm) & npts == i & dgp == "morris", alpha]))
}

