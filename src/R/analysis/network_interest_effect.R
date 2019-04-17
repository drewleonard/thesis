rm(list = ls())
setwd("~/Documents/thesis/data/")

source('../src/R/analysis/analysis_helper.R')

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

# Cluster topic prevalence analysis
clusterPrep <- estimateEffect(
  formula = c(1:49) ~ AccountGroupCluster,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

clusterPrepPlot <- plot(
  clusterPrep,
  covariate = "AccountGroupCluster",
  topics = c(1:49),
  model = stm,
  method = "difference",
  cov.value1 = "cluster_2",
  cov.value2 = "cluster_1",
  xlab = "Cluster one ... Cluster two",
  main = "",
  labeltype = "custom",
  xlim = c(-.3, .3),
  custom.labels = topicNames
)

clusterPrepPlotDf <-
  data.frame(t(sapply(clusterPrepPlot$cis, function(x)
    x[1:max(lengths(clusterPrepPlot$cis))])))
clusterPrepDf <- data.frame(
  clusterPrepPlot$labels,
  clusterPrepPlot$topics,
  unlist(clusterPrepPlot$means),
  clusterPrepPlotDf$X2.5.,
  clusterPrepPlotDf$X97.5.
)
colnames(clusterPrepDf) <-
  c("primary_topic", "topic_n", "mean", "lower", "upper")
clusterPrepDf <- clusterPrepDf %>%
  group_by(primary_topic) %>%
  summarise(
    avg_ci_point_estimate = mean(mean),
    avg_ci_lower_bound = mean(lower),
    avg_ci_upper_bound = mean(upper)
  )

table_text <- cbind(
  c(NA, seq(1, length(
    clusterPrepDf$primary_topic
  ))),
  c("Primary Topic", as.vector(clusterPrepDf$primary_topic)),
  c(
    "Confidence Interval (95%)",
    formatInterval(
      clusterPrepDf$avg_ci_point_estimate,
      clusterPrepDf$avg_ci_lower_bound,
      clusterPrepDf$avg_ci_upper_bound
    )
  )
)

pdf("~/Documents/thesis/data/figures/analysis/network_interest_effect.pdf")
forestplot(
  table_text,
  graph.pos = 3,
  is.summary = c(TRUE, rep(
    FALSE, length(clusterPrepDf$primary_topic)
  )),
  align = c("r", "l", "r"),
  mean = c(NA, clusterPrepDf$avg_ci_point_estimate),
  lower = c(NA, clusterPrepDf$avg_ci_lower_bound),
  upper = c(NA, clusterPrepDf$avg_ci_upper_bound),
  fn.ci_norm = fpDrawCircleCI,
  hrzl_lines = list("2" = gpar(lwd = 2, col = "#000000")),
  boxsize = .15,
  xlab = ("\nCluster One ... Cluster Two"),
  cex = 0,
  zero = 0,
  new_page = FALSE,
  txt_gp = fpTxtGp(
    xlab = gpar(cex = .75),
    label = gpar(cex = .75),
    ticks = gpar(cex = .75)
  ),
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  cex = 0.9,
  lineheight = "auto",
  colgap = unit(4, "mm"),
  lwd.ci = 1,
  clip = c(-.1, 1)
)
dev.off()
