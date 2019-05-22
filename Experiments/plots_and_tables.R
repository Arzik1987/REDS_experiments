library(reshape)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(data.table)
library(batchtools)

dir.create(file.path(getwd(), "plots_tables/main_pics"), showWarnings = FALSE)
dir.create(file.path(getwd(), "plots_tables/main_tex"), showWarnings = FALSE)
dir.create(file.path(getwd(), "plots_tables/appendix"), showWarnings = FALSE)

load(paste0(getwd(),"/results/res.RData"))
load(paste0(getwd(),"/results/res_f.RData"))
load(paste0(getwd(),"/results/peeling.RData"))
load(paste0(getwd(),"/dim.RData"))

#########################
#####           #########
#####   PLOTS   #########
#####           #########
#########################

#### aggregate boxplots

plot.dgpwize <- function(d, np, coef = 100, lbl = "AUC"){
  
  d <- d[npts == np & (is.na(noise) | noise == 0) & (is.na(ngen) | ngen == 100000),]
  if(lbl == "AUC"){
    d <- d[, .(algorithm, m.auc)]
  } else if(lbl == "density"){
    d <- d[, .(algorithm, m.dens)]
  } else if(lbl == "# restricted"){
    d <- d[, .(algorithm, m.interp)]
  } else if(lbl == "consistency"){
    d <- d[, .(algorithm, m.consist)]
  } else {
    stop("undefined label lbl")
  }
  d[, 2] <- d[, 2]*coef
  names(d) <- c("variable", "value")

  cols <- brewer.pal(6, "Paired")
  bs = 9
  
  p <- ggplot(d, aes(x = variable, y = value, fill = variable)) +
    geom_boxplot(lwd = 0.4) +
    scale_fill_manual("", values = cols) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0.1,0,0,0), "cm"),
          panel.grid.minor = element_blank())+
    ylab(lbl)
  
  p
}
nms <- c("auc_train", "prec_train", "interp", "consist_train")

for(i in 1:4){
  
  coef = 100
  if(i == 1) lbl = "AUC"
  if(i == 2) lbl = "density"
  if(i == 3) {lbl = "# restricted"; coef = 1}
  if(i == 4) lbl = "consistency"
  
  p <- plot.dgpwize(res, np = 400, coef = coef, lbl = lbl)
  ggsave(paste0(getwd(), "/plots_tables/main_pics/", nms[i], "_400.pdf"), 
         plot = p, device = cairo_pdf, width = 3, height = 1.1)
  
}

#### DSGC lowess curves

plot.curves <- function(d, f = 0.2){
  
  for(i in unique(d$algorithm)){
    tmp <- lowess(d[algorithm == i, 1:2], f = f)
    d[algorithm == i, 1] <- tmp[[1]]
    d[algorithm == i, 2] <- tmp[[2]]
  }
  
  cols <- brewer.pal(6, "Paired")
  bs = 9
  
  p <- ggplot(d, aes(x = coverage, y = density, col = algorithm, linetype = algorithm)) +
    geom_line() +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = c(0.5, 1.1), 
          legend.title = element_blank(), 
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          plot.margin = unit(c(0.4,0,0,0.12), "cm"),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(nrow = 1))

  p
}

p <- plot.curves(peeling)

ggsave(paste0(getwd(), "/plots_tables/main_pics/lowess_DSGC_400.pdf"), 
       plot = p, device = cairo_pdf, width = 3, height = 1.3)


#### DSGC quality boxes

plot.dsgc.boxes <- function(d){
  
  d <- d[problem == "dsgc" & (is.na(noise) | noise == 0) & (is.na(ngen) | ngen == 100000) & npts == 400,]
  
  cols <- brewer.pal(6, "Paired")
  bs = 9
  
  p1 <- ggplot(d, aes(x = algorithm, y = auc, fill = algorithm)) +
    geom_boxplot(lwd = 0.4) +
    scale_fill_manual("", values = cols) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          panel.grid.minor = element_blank())+
    ylab("AUC")
  
  p2 <- ggplot(d, aes(x = algorithm, y = dens, fill = algorithm)) +
    geom_boxplot(lwd = 0.4) +
    scale_fill_manual("", values = cols) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0.12), "cm"),
          panel.grid.minor = element_blank()) +
    ylab("density")
  
  p3 <- ggplot(d, aes(x = algorithm, y = interp, fill = algorithm)) +
    geom_boxplot(lwd = 0.4) +
    scale_fill_manual("", values = cols) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          panel.grid.minor = element_blank()) +
    ylab("# restricted")
  
  list(p1, p2, p3)
}

tmp <- plot.dsgc.boxes(res.f)

ggsave(paste0(getwd(), "/plots_tables/main_pics/DSGC_400.pdf"), 
       plot = grid.arrange(
         grobs = tmp,
         layout_matrix = rbind(1, 2, 3)
       ), device = cairo_pdf, width = 3, height = 3)


#### DSGC npts

plot.dsgc.np <- function(d){
  d <- d[problem == "dsgc" & (is.na(noise) | noise == 0) & (is.na(ngen) | ngen == 100000),]
  
  cols <- brewer.pal(6, "Paired")
  bs = 9
  npts = c(200, 400, 800, 1200, 2000)
  
  p1 <- ggplot(d, aes(x = npts, y = m.auc, col = algorithm, linetype = algorithm)) +
    geom_line() +
    ylab("AUC") +
    scale_x_continuous(breaks = npts, labels = npts, trans = "log2") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = c(0.5, 1.1), 
          legend.title = element_blank(), 
          axis.title.x = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          plot.margin = unit(c(0.4,0,0,0), "cm"),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(nrow = 1))
  
  p2 <- ggplot(d, aes(x = npts, y = m.dens, col = algorithm, linetype = algorithm)) +
    geom_line() +
    ylab("density") +
    scale_x_continuous(breaks = npts, labels = npts, trans = "log2") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          plot.margin = unit(c(0,0,0,0.12), "cm"),
          panel.grid.minor = element_blank())
  
  p3 <- ggplot(d, aes(x = npts, y = m.consist, col = algorithm, linetype = algorithm)) +
    geom_line() +
    ylab("consistency") +
    scale_x_continuous(name = "number of points in d", breaks = npts, labels = npts, trans = "log2") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = "none",
          plot.margin = unit(c(0,0,0,0.12), "cm"),
          panel.grid.minor = element_blank())
  
  list(p1, p2, p3)
}

tmp <- plot.dsgc.np(res)

ggsave(paste0(getwd(), "/plots_tables/main_pics/DSGC_npts.pdf"), 
       plot = grid.arrange(
         grobs = tmp,
         heights = c(1.1, 1, 1.15),
         layout_matrix = rbind(1, 2, 3)
       ), device = cairo_pdf, width = 3, height = 3)

#### DSGC ngen


plot.dsgc.ngen <- function(d){
  d <- d[problem == "dsgc" & (is.na(noise) | noise == 0) & npts == 400,]
  d[is.na(d$ngen), ngen := 200]
  ng <- unique(d$ngen)
  tmp <- d
  
  cols <- brewer.pal(6, "Paired")
  bs = 9
  
  d <- data.table(cast(tmp, ngen~algorithm, mean, value = "m.auc"))
  d[, B := d$B[1]]
  d[, B.all := d$B.all[1]]
  d[, O := d$O[1]]
  d[, O.p := d$O.p[1]]
  d <- melt(d, id = "ngen")
  
  p1 <- ggplot(d, aes(x = ngen, y = value, col = variable, linetype = variable)) +
    geom_line() +
    ylab("AUC") +
    scale_x_continuous(breaks = ng, labels = ng, trans = "log2") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = c(0.5, 1.1), legend.title = element_blank(), axis.title.x = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          plot.margin = unit(c(0.4,0.05,0,0), "cm"),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(nrow = 1))
  
  d <- data.table(cast(tmp, ngen~algorithm, mean, value = "m.dens"))
  d[, B := d$B[1]]
  d[, B.all := d$B.all[1]]
  d[, O := d$O[1]]
  d[, O.p := d$O.p[1]]
  d <- melt(d, id = "ngen")
  
  p2 <- ggplot(d, aes(x = ngen, y = value, col = variable, linetype = variable)) +
    geom_line() +
    ylab("density") +
    xlab("K value") +
    scale_x_continuous(breaks = ng, labels = ng, trans = "log2") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = "none",
          plot.margin = unit(c(0,0.05,0,0), "cm"),
          panel.grid.minor = element_blank())
  
  list(p1, p2)
}

tmp <- plot.dsgc.ngen(res)

ggsave(paste0(getwd(), "/plots_tables/main_pics/DSGC_ngen.pdf"), 
       plot = grid.arrange(
         grobs = tmp,
         heights = c(1, 1),
         layout_matrix = rbind(1, 2)
       ), device = cairo_pdf, width = 3, height = 2)


#### DSGC noise

plot.dsgc.noise <- function(d){
  cols <- brewer.pal(6, "Paired")
  bs = 9
  
  d <- d[problem == "dsgc" & npts == 400 & (is.na(ngen) | ngen == 100000),]

  p1 <- ggplot(d, aes(x = noise, y = m.auc, col = algorithm, linetype = algorithm)) +
    geom_line() +
    ylab("AUC") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = c(0.5, 1.1), legend.title = element_blank(), axis.title.x = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          plot.margin = unit(c(0.4,0,0,0), "cm"),
          panel.grid.minor = element_blank()) +
    guides(col = guide_legend(nrow = 1))
  
  p2 <- ggplot(d, aes(x = noise, y = m.dens, col = algorithm, linetype = algorithm)) +
    geom_line() +
    ylab("density") +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 2, 2, 3, 3)) +
    theme_light(base_size = bs) +
    theme(legend.position = "none",
          plot.margin = unit(c(0,0,0,0.12), "cm"),
          panel.grid.minor = element_blank())
  
  list(p1, p2)
}

tmp <- plot.dsgc.noise(res)

ggsave(paste0(getwd(), "/plots_tables/main_pics/DSGC_noise.pdf"), 
       plot = grid.arrange(
         grobs = tmp,
         heights = c(1, 1),
         layout_matrix = rbind(1, 2)
       ), device = cairo_pdf, width = 3, height = 2)


#########################
#####           #########
#####   TABLES  #########
#####           #########
#########################

get.mask <- function(d, best = "max", np = c(400, 800, 1600), value = "auc", 
                     coef = 100, rn = 1, addvol = FALSE, add.dim = FALSE, d.dim){
  
  d <- d[npts %in% np & (is.na(noise) | noise == 0) & (is.na(ngen) | ngen == 100000),]
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
    
    leftside <- matrix(c("", "avg", np[j], "\\# 1", " ", "\\# 2"), ncol = 2, byrow = TRUE)
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
    
    stabs[[j]] <- cbind(leftside, rbind(b0, b1, b2))
    colnames(stabs[[j]]) <- c("npts", " ", colnames(mask))
  }
  
  res.excel <- tmp[,, 1]
  for(j in 2:length(np)){
    tabs[[1]] <- cbind(tabs[[1]], tabs[[j]])
    stabs[[1]] <- rbind(stabs[[1]], stabs[[j]])
    res.excel <- cbind(res.excel, tmp[,, j]) 
  }
  write.csv(res.excel, paste0(getwd(), "/plots_tables/appendix/", value, "_tabs.csv"))
  
  if(add.dim){
    n = nrow(tabs[[1]]) - nrow(d.dim)
    tmp <- rbindlist(list(d.dim[, 2:3], data.table(dim = rep(" ", n), inf.dim = "")))
    colnames(tmp) <- c("D", "I")
    rnames <- row.names(tabs[[1]])
    tabs[[1]] <- cbind(tabs[[1]], tmp)
    row.names(tabs[[1]]) <- rnames
  }

  write.table(tabs[[1]], paste0(getwd(), "/plots_tables/appendix/", value, "_tabs.txt"), 
              quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = TRUE)
  write.table(stabs[[1]], paste0(getwd(), "/plots_tables/main_tex/", value, "_stabs.txt"), 
              quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)
}

get.mask(res, best = "max", value = "auc", coef = 100, rn = 1, addvol = FALSE)
get.mask(res, best = "max", value = "dens", coef = 100, rn = 1, addvol = FALSE)
get.mask(res, best = "min", value = "interp", coef = 1, rn = 2, addvol = FALSE, add.dim = TRUE, d.dim = d.dim)
get.mask(res, best = "max", value = "consist", coef = 100, rn = 1, addvol = TRUE)


#### restricted dimensions

d <- d.dim[dim > inf.dim,]
res[is.na(dgp), dgp := "dsgc"]
d <- ijoin(d, res, by = "dgp")
d <- d[npts %in% c(400, 800, 1600) & (is.na(ngen) | ngen == 100000) & (is.na(noise) | noise == 0),]
d[, dim.r := m.interp - inf.dim]
d[dim.r < 0, dim.r := 0]

d <- d[, .(restricted = mean(dim.r)), by = c("npts", "algorithm")]
d <- round(cast(d, npts ~ algorithm, value = "restricted"), 2)
colnames(d)[1] <- "\\lvert d\\rvert"
write.table(d, paste0(getwd(), "/plots_tables/main_tex/iir_dim.txt"), 
            quote = FALSE, eol = "\\\\\n", sep = " & ", row.names = FALSE)

