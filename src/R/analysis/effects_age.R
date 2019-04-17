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

prepAgeQuant <- estimateEffect(
  formula = 1:49 ~ AgeAverage,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

pdf('~/Documents/thesis/data/figures/analysis/extra/effects_age.pdf')
plot(
  prepAgeQuant,
  "AgeAverage",
  method = "continuous",
  topics = c(17, 8),
  model = stm,
  printlegend = F,
  linecol = c("blue", "red"),
  xlab = "Targeted Age (Average)",
  labeltype = "custom",
  custom.labels = c("Police Brutality", "Race Tensions")
)
legend(
  x = 41,
  y = -0.04,
  c("Police Brutality", "Race Tensions"),
  lwd = 2,
  cex = 1,
  col = c("blue", "red")
)
dev.off()
