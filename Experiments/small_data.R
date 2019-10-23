
library(primre)
library(caret)
library(lhs)
library(ggplot2)
library(gridExtra)

#### conventional

set.seed(22)

dgp <- "3"
n.points <- 100
box <- matrix(c(rep(0, 5), rep(1, 5)), nrow = 2, byrow = TRUE)
d <- get.labs.box(box = box, n.points = n.points, dgp = dgp, laths = TRUE)

resn <- norm.prim(dtrain = d, dtest = d, box = box, minpts = 20, pasting = TRUE)
bxn <- resn[[3]][[length(resn[[3]])]]

dn <- d
dn <- as.data.frame(cbind(dn[[1]], dn[[2]]))
colnames(dn) <- c(paste0("x", paste0(1:5)), "y") 

#### RF

dp <- list()
dp[[1]] <- randomLHS(1000, 5)
for(i in 1:5){
  d.width <- box[2, i] - box[1, i]
  dp[[1]][, i] <- dp[[1]][, i]*d.width + box[1, i]
}
colnames(d[[1]]) <- colnames(dp[[1]]) <- paste0("x", paste0(1:5))
d[[2]] <- as.factor(d[[2]])
res.rf <- train(d[[1]], d[[2]], method = "rf")

#### data

dp[[2]] <- predict(res.rf, dp[[1]])
dp[[2]] <- as.numeric(as.character(dp[[2]]))
res <- norm.prim(dtrain = dp, dtest = dp, box = box, minpts = 20)
dppred <- as.data.frame(cbind(dp[[1]], dp[[2]]))
colnames(dppred) <- c(paste0("x", paste0(1:5)), "y") 
bx <- res[[3]][[length(res[[3]])]]

dp[[2]] <- predict(res.rf, dp[[1]], type = "prob")[, 2]
dpprob <- as.data.frame(cbind(dp[[1]], dp[[2]]))
colnames(dpprob) <- c(paste0("x", paste0(1:5)), "y")

#### plotting

ps = 0.25
ls = 0.25
bs = 7

p1 <- ggplot(dn, aes(x = x1, y = x2, color = y)) + geom_point(size = ps) + scale_colour_gradient2(high = "black") +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1, ymin = 0.8, ymax = 1), alpha = 0, color = "orange", linetype = 2 , size = ls*2) +
  geom_rect(mapping = aes(xmin = bxn[1, 1], xmax = bxn[2, 1], ymin = bxn[1, 2], ymax = bxn[2, 2]), alpha = 0, color = "black", 
            size = ls) +
  theme_grey(base_size = bs) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  ggtitle("PRIM")

p2 <- ggplot(dn, aes(x = x3, y = x4, color = y)) + geom_point(size = ps) + scale_colour_gradient2(high = "black") +
  geom_rect(mapping = aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), alpha = 0, color = "orange", linetype = 2 , size = ls*2) +
  geom_rect(mapping = aes(xmin = bxn[1, 3], xmax = bxn[2, 3], ymin = bxn[1, 4], ymax = bxn[2, 4]), alpha = 0, color = "black",
            size = ls) +
  theme_grey(base_size = bs) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  ggtitle("PRIM")

p3 <- ggplot(dpprob, aes(x = x1, y = x2, color = y)) + geom_point(size = ps) + scale_colour_gradient2(high = "black") +
  geom_rect(mapping = aes(xmin = 0.6, xmax = 1, ymin = 0.8, ymax = 1), alpha = 0, color = "orange", linetype = 2 , size = ls*2) +
  geom_rect(mapping = aes(xmin = bx[1, 1], xmax = bx[2, 1], ymin = bx[1, 2], ymax = bx[2, 2]), alpha = 0, color = "black",
            size = ls) +
  theme_grey(base_size = bs) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  ggtitle("REDS")

p4 <- ggplot(dpprob, aes(x = x3, y = x4, color = y)) + geom_point(size = ps) + scale_colour_gradient2(high = "black") +
  geom_rect(mapping = aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), alpha = 0, color = "orange", linetype = 2 , size = ls*2) +
  geom_rect(mapping = aes(xmin = bx[1, 3], xmax = bx[2, 3], ymin = bx[1, 4], ymax = bx[2, 4]), alpha = 0, color = "black",
            size = ls) +
  theme_grey(base_size = bs) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  ggtitle("REDS")


ggsave(paste0(getwd(), "/plots_tables/Fig_12.pdf"), 
       plot = grid.arrange(p1, p2, p3, p4, nrow = 2), device = cairo_pdf, width = 3.3, height = 3.3)

