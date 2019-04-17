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

# PLOT _
# Plot group network
# THESE WERE COMPUTED WITH JUPYTER NOTEBOOK
edges <- read.csv("csv/group_to_group.csv")
nodes <- read.csv("csv/group_node.csv")

edges_spread <- edges %>%
  spread(group2, weight)
edges_spread[is.na(edges_spread)] <- 0

cormatrix <- cor_auto(edges_spread)

# Matrix heatmap
cormatrix_original_labels <- colnames(cormatrix)
cormatrix_new_labels <- sprintf("%s", seq(1:60))
colnames(cormatrix) <- cormatrix_new_labels
rownames(cormatrix) <- cormatrix_new_labels

plot <- Heatmap(
  cormatrix,
  clustering_distance_rows = function(x)
    as.dist(1 - cor(t(x))),
  clustering_distance_columns = function(x)
    as.dist(1 - cor(t(x))),
  clustering_method_columns = "average",
  clustering_method_rows = "average",
  row_dend_reorder = TRUE,
  column_dend_reorder = TRUE,
  row_dend_width = unit(2, "in"),
  column_dend_height = unit(2, "in"),
  name = " ",
  heatmap_legend_param = list(
    legend_width = unit(5, "in"),
    legend_direction = "horizontal"
  )
)

pdf("~/Documents/thesis/data/figures/analysis/network_interest_heatmap.pdf",
    height = 14,
    width = 14)
draw(plot, heatmap_legend_side = "bottom")
dev.off()