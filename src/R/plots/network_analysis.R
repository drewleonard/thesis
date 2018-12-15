rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(tidyverse)
library(formatR)
library(cowplot)
library(qgraph)

# Read data
fb <- read.csv("csv/fb_gold.csv")

# Process data
processed <- textProcessor(documents = fb$AdText, metadata = fb)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Load full, stm
full <- read.csv("csv/df_interests_spline_age_49.csv")
stm <- readRDS("~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")

# Set topic names
topicNames <- c(
  "Race Tensions", "Black Empowerment", "Black Empowerment",
  "Black Empowerment", "Incarceration", "Communal Support",
  "Race Tensions", "Race Tensions", "Mixed",
  "Islam in America", "Race Tensions", "Black Empowerment",
  "Black Empowerment", "Mixed", "Mixed",
  "Police Brutality", "Police Brutality", "Race Tensions",
  "Police Brutality", "Patriotism", "Bearing Arms",
  "Social Justice", "Mixed", "Mixed",
  "Music Streaming", "Patriotism", "Black Empowerment",
  "Black Empowerment", "Race Tensions", "Mixed",
  "Social Justice", "Black Empowerment", "Black Empowerment",
  "National Security", "Police Brutality", "Race Tensions",
  "Election", "Patriotism", "Black Empowerment",
  "Social Justice", "Minorities", "Race Tensions",
  "Police Brutality", "Patriotism", "Race Tensions",
  "Black Empowerment", "Black Empowerment", "Minorities",
  "Black Empowerment"
)

edges <- read.csv("csv/group_to_group_topic.csv")
nodes <- read.csv("csv/group_node_topic.csv")

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
  mutate(rank = max((log2(rank) / 1.5), 2))
  mutate(rank = max((log2(rank) / 2) + 1.5, 2))

nums <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
  "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
  "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
  "51", "52", "53", "54", "55", "56", "57", "58", "59", "60",
  "61", "62", "63", "64", "64", "66"
)

qgraph(cormatrix, 
       layout = "spring",
       graph = "glasso",
       sampleSize = nrow(cormatrix),
       theme = "Borkulo",
       nodeNames = colnames(cormatrix),
       labels = nums,
       vsize = group_sizes$rank,
       filetype = "pdf",
       filename = "~/Documents/coolplot-topic",
       cut = .25,
       minimum = 0.1,
       width = 17.5,
       border.width = 1.5,
       label.cex = 1.25,
       height = 17.5,
       legend = TRUE,
       legend.mode = "names",
       normalize = TRUE,
       threshold = TRUE,
       # XKCD = TRUE, # the most important argument
       theme = "Borkulo")
