rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(ComplexHeatmap)
library(RColorBrewer)
library(cowplot)
library(dendsort)
library(dendsort)
library(extrafont)
library(forestplot)
library(formatR)
library(ggrepel)
library(ggridges)
library(gplots)
library(lattice)
library(latticeExtra)
library(lubridate)
library(qgraph)
library(reshape2)
library(scales)
library(stm)
library(styler)
library(tidyverse)
library(viridis)

# Read data
fb <- read.csv("csv/fb_gold.csv")

# Process data
processed <- textProcessor(documents = fb$AdText, metadata = fb)
out <-
  prepDocuments(processed$documents, processed$vocab, processed$meta)

# Load full, stm
full <- read.csv("csv/df_interests_spline_age_49.csv")
stm <-
  readRDS("~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")

# Plot groups relative to topics
# (Heatmap)
accountGroupTopicDf <- full %>%
  select(c("AccountGroup", "primary_topic")) %>%
  filter(AccountGroup != "unavailable" & primary_topic != "Mixed")

accountGroupTopicMatrix <-
  t(as.matrix(table(droplevels(
    accountGroupTopicDf
  ))))

d2 <-
  ifelse(log10(accountGroupTopicMatrix) < 0,
         0,
         log10(accountGroupTopicMatrix))

dd2.row <- as.dendrogram(hclust(dist(d2)))
row2.ord <- order.dendrogram(dd2.row)
dd2.col <- as.dendrogram(hclust(dist(t(d2))))
col2.ord <- order.dendrogram(dd2.col)

lattice.options(axis.padding = list(factor = 0.5))

pdf('~/Documents/thesis/data/figures/analysis/topic_account_heatmap.pdf')
levelplot(
  d2[row2.ord, col2.ord],
  aspect = "fill",
  xlab = "Discussed Topics",
  ylab = "Account Groups",
  pretty = TRUE,
  drop.unused.levels = TRUE,
  scales = list(x = list(rot = 45), tck = c(0, 0)),
  colorkey = list(space = "right"),
  par.settings = custom.theme(region = plasma(10)),
  border = "black",
  border.lwd = .6,
  xaxt = "n",
  yaxt = "n",
  legend = list(
    left = list(
      fun = dendrogramGrob,
      args =
        list(
          x = dd2.col,
          ord = col2.ord,
          side = "right",
          size = 0
        )
    ),
    top = list(
      fun = dendrogramGrob,
      args =
        list(x = dd2.row,
             side = "top", size = 0)
    )
  )
)
dev.off()