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

# Load fonts
extrafont::loadfonts()

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
# Plot topics relative to interests
# (Heatmap)
interestTopicDf <- full %>%
  select(c("Interests", "primary_topic")) %>%
  filter(Interests != "mixed" & Interests != "Music" &
           Interests != "unavailable" & primary_topic != "Mixed" & primary_topic != "Music Streaming")

interestTopicMatrix <-
  t(as.matrix(table(droplevels(interestTopicDf))))

d <-
  ifelse(log(interestTopicMatrix) < 0, 0, log(interestTopicMatrix))

dd.row <- as.dendrogram(hclust(dist(d)))
row.ord <- order.dendrogram(dd.row)
dd.col <- as.dendrogram(hclust(dist(t(d))))
col.ord <- order.dendrogram(dd.col)

lattice.options(axis.padding = list(factor = 0.5))

pdf("~/Documents/thesis/data/figures/analysis/topic_interest_heatmap.pdf")
levelplot(
  d[row.ord, col.ord],
  aspect = "fill",
  xlab = "Discussed Topics",
  ylab = "Targeted Interests",
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
          x = dd.col,
          ord = col.ord,
          side = "right",
          size = 0
        )
    ),
    top = list(fun = dendrogramGrob, args =
                 list(
                   x = dd.row,
                   side = "top", size = 0
                 ))
  )
)
dev.off()