
list.of.packages <- c("reshape", "ggplot2", "gridExtra", "RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(reshape)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(data.table)
library(batchtools)


dir.create(file.path(getwd(), "plots_tables"), showWarnings = FALSE)

load(paste0(getwd(),"/results/res.RData"))
load(paste0(getwd(),"/results/res_f.RData"))
load(paste0(getwd(),"/results/peeling_TGL.RData"))
load(paste0(getwd(),"/results/peeling_lake.RData"))



#### Figure 13. Peeling trajectories

plot.curves <- function(d, f = 0.4, label = "", coly = "black"){
  
  for(i in unique(d$algorithm)){
    tmp <- lowess(d[algorithm == i, 1:2], f = f)
    d[algorithm == i, 1] <- tmp[[1]]
    d[algorithm == i, 2] <- tmp[[2]]
  }
  
  cols <- brewer.pal(7, "Paired")[c(1,2,5,6)]
  bs = 9
  
  d$algorithm <- factor(d$algorithm, levels = c("P", "Pc", "RPf", "RPfp", "RPx", "RPxp"), ordered = TRUE)
  
  p <- ggplot(d, aes(x = recall, y = precision, col = algorithm, linetype = algorithm)) +
    geom_line(size = 0.5) +
    scale_color_manual("", values = cols) +
    scale_linetype_manual("", values = c(1, 1, 3, 3)) +
    theme_light(base_size = bs) +
    theme(
      legend.position = c(0.5, 0.9),
      legend.title = element_blank(),
      legend.text = element_text(size = bs - 1),
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


p1 <- plot.curves(peeling.TGL[!(algorithm %in% c("RPx", "RPxp"))], label = "TGL", f = 0.3)
p2 <- plot.curves(peeling.lake[!(algorithm %in% c("RPx", "RPxp"))], label = "lake", f = 0.1, coly = "white")
mylegend <- g_legend(p1)
p3 <- grid.arrange(mylegend,
                   arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow = 1), nrow = 2, heights = c(1, 7))

ggsave(paste0(getwd(), "/plots_tables/Fig_13_tp_peeling.pdf"), 
       plot = p3, device = cairo_pdf, width = 3, height = 1.2)




#### TABLES

tmp <- res[problem == "TGL"& algorithm %in% c("Pc", "RPf", "RPfp"), 
           c("algorithm", "m.auc", "m.prec", "m.consist", "m.interp")]
tmp[, 2:4] <- round(tmp[, 2:4]*100, 1)
tmp[, 5] <- round(tmp[, 5], 2)
colnames(tmp) <- c("", "PR AUC", "precision", "consistency", "# restricted")
tmp <- t(tmp)
# write.table(tmp, paste0(getwd(), "/plots_tables/TGL.txt"), 
            # quote = FALSE, eol = "\\\\ \\hline\n", sep = " & ", row.names = TRUE, col.names = FALSE)
write.csv(tmp, paste0(getwd(), "/plots_tables/TGL.csv"))


tmp <- res[problem == "lake" & algorithm %in% c("Pc", "RPf", "RPfp"), 
           c("algorithm", "m.auc", "m.prec", "m.consist", "m.interp")]
tmp[, 2:4] <- round(tmp[, 2:4]*100, 1)
tmp[, 5] <- round(tmp[, 5], 2)
colnames(tmp) <- c("", "PR AUC", "precision", "consistency", "# restricted")
tmp <- t(tmp)
# write.table(tmp, paste0(getwd(), "/plots_tables/lake.txt"), 
# quote = FALSE, eol = "\\\\ \\hline\n", sep = " & ", row.names = TRUE, col.names = FALSE)
write.csv(tmp, paste0(getwd(), "/plots_tables/lake.csv"))






# # #### reproduce reference plots and see REDS in work
# 
# source("read_data.R")
# 
# #### TGL 
# 
# train <- list()
# train[[1]] <- dTGL[,1:9]
# train[[2]] <- dTGL[,10]
# box <- matrix(c(67, 450, 0, 80, 0.2, -0.8, 0, -0.1,90,
#          134, 1000,	1, 100, 0.6, -0.2, 2, 0.1, 200), nrow = 2, byrow = TRUE)
# 
# res <- norm.prim(dtrain = train, dtest = NULL, box = box, peel.alpha = 0.1)
# res.rf <- reds.prim(dtrain = train, dtest = train, box = box, npts = 100000, distr = "laths", meth = "rf")
# 
# plot(res[[2]][,1], res[[2]][,2], xlim = c(0,1), ylim = c(0,1))
# lines(res.rf[[2]][,1], res.rf[[2]][,2], col = "red")
# 
# res$boxes
# res.rf$boxes.pred[length(res.rf$boxes.pred)]
# res.rf$boxes.prob[length(res.rf$boxes.prob)]
# 
# #### lake
# 
# box <- matrix(c(0.1, 0.93, 0.01, 2, 0.001,
#          0.45, 0.99, 0.05, 4.5, 0.005), nrow = 2, byrow = TRUE)
# 
# res <- norm.prim(dtrain = dl, dtest = NULL, box = box, peel.alpha = 0.05)
# res.rf <- reds.prim(dtrain = dl, dtest = dl, box = box, npts = 100000, distr = "laths", meth = "rf")
# 
# plot(res[[2]][,1], res[[2]][,2], xlim = c(0,1), ylim = c(0,1))
# lines(res.rf[[2]][,1], res.rf[[2]][,2], col = "red")
# 
# res$boxes[length(res$boxes)]
# res.rf$boxes.pred[length(res.rf$boxes.pred)]
# res.rf$boxes.prob[length(res.rf$boxes.prob)]
# 
# # #### END TEST
# 
