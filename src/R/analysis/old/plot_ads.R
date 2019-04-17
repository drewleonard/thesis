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

# Set topic names
topicNames <- c(
  "Race Tensions",
  "Black Empowerment",
  "Black Empowerment",
  "Black Empowerment",
  "Incarceration",
  "Communal Support",
  "Race Tensions",
  "Race Tensions",
  "Mixed",
  "Islam in America",
  "Race Tensions",
  "Black Empowerment",
  "Black Empowerment",
  "Mixed",
  "Mixed",
  "Police Brutality",
  "Police Brutality",
  "Race Tensions",
  "Police Brutality",
  "Patriotism",
  "Bearing Arms",
  "Social Justice",
  "Mixed",
  "Mixed",
  "Music Streaming",
  "Patriotism",
  "Black Empowerment",
  "Black Empowerment",
  "Race Tensions",
  "Mixed",
  "Social Justice",
  "Black Empowerment",
  "Black Empowerment",
  "National Security",
  "Police Brutality",
  "Race Tensions",
  "Election",
  "Patriotism",
  "Black Empowerment",
  "Social Justice",
  "Minorities",
  "Race Tensions",
  "Police Brutality",
  "Patriotism",
  "Race Tensions",
  "Black Empowerment",
  "Black Empowerment",
  "Minorities",
  "Black Empowerment"
)

# PLOT _
# Plot interest effects
# prepInterests <- estimateEffect(
#   formula = 1:49 ~ Interests,
#   stmobj = stm,
#   metadata = out$meta,
#   uncertainty = "Global"
# )
# 
# topicNamesDf <- data.frame(
#   topicnames = topicNames,
#   TopicNumber = 1:49,
#   TopicProportions = colMeans(stm$theta)
# )
# 
# interestsAllUnordered <- plot(
#   prepInterests,
#   covariate = "Interests",
#   topics = c(1:49),
#   model = stm,
#   method = "difference",
#   cov.value1 = "Right Wing",
#   cov.value2 = "Left Wing",
#   xlab = "Left wing ... Right wing",
#   main = ""
# )
# rank <- order(unlist(interestsAllUnordered$means))
# topicNamesDf <- topicNamesDf[rank,]
