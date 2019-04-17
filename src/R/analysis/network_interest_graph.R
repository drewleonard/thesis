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

group_sizes <- full %>%
  select("AccountGroup") %>%
  group_by(AccountGroup) %>%
  summarise(rank = n()) %>%
  ungroup() %>%
  filter(AccountGroup %in% colnames(cormatrix)) %>%
  rowwise() %>%
  mutate(rank = max((log2(rank) / 2) + 1, 2))

nums <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "11",
  "12",
  "13",
  "14",
  "15",
  "16",
  "17",
  "18",
  "19",
  "20",
  "21",
  "22",
  "23",
  "24",
  "25",
  "26",
  "27",
  "28",
  "29",
  "30",
  "31",
  "32",
  "33",
  "34",
  "35",
  "36",
  "37",
  "38",
  "39",
  "40",
  "41",
  "42",
  "43",
  "44",
  "45",
  "46",
  "47",
  "48",
  "49",
  "50",
  "51",
  "52",
  "53",
  "54",
  "55",
  "56",
  "57",
  "58",
  "59",
  "60"
)

qgraph(
  cormatrix,
  layout = "spring",
  theme = "Borkulo",
  nodeNames = colnames(cormatrix),
  labels = nums,
  vsize = group_sizes$rank,
  filetype = "pdf",
  filename = "~/Documents/thesis/data/figures/analysis/network_interest_graph.pdf",
  cut = .9,
  minimum = .5,
  width = 17.5,
  height = 17.5,
  legend = TRUE,
  legend.mode = "names",
  normalize = TRUE,
  theme = "Borkulo"
)

