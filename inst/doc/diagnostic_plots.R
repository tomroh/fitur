## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 7)
library(fitur)
library(ggplot2)

## ----stats---------------------------------------------------------------
set.seed(438)
x <- rweibull(10000, shape = 5, scale = 1)

## ----histPlot------------------------------------------------------------
dt <- data.frame(x)
nbins <- 30
g <- ggplot(dt, aes(x)) +
  geom_histogram(aes(y = ..density..), 
                bins = nbins, fill = NA, color = "black") +
  theme_bw() +
  theme(panel.grid = element_blank())
g

## ----densPlot------------------------------------------------------------
dists <- c('gamma', 'lnorm', 'weibull')
cols <- c('red', 'blue', 'yellow')
multipleFits <- lapply(dists, fit_univariate, x = x)
for (i in 1:length(multipleFits)) {
  g <- g +
    stat_function(fun = multipleFits[[i]][[1]],
                  aes_(color = dists[i]),
                  size = 1)
}
g +
  scale_color_discrete(name = "distribution",
                     #values = cols, 
                     breaks = dists,
                     labels = paste0('d', dists))

## ----qqplot--------------------------------------------------------------
plot_qq(x, multipleFits) +
  theme_bw() +
  theme(panel.grid = element_blank())

## ----ppplot--------------------------------------------------------------
plot_pp(x, multipleFits) +
  theme_bw() +
  theme(panel.grid = element_blank())

