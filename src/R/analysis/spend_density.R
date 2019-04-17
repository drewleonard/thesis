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

# Basic summary graphs
metric_df <- full %>%
  select(Impressions, Clicks, AdSpend) %>%
  filter(Impressions < quantile(Impressions, 0.98) &
           Impressions > 0) %>%
  filter(Clicks < quantile(Clicks, 0.98) & Clicks > 0) %>%
  filter(AdSpend < quantile(AdSpend, 0.98) & AdSpend > 0)

pdf("~/Documents/thesis/data/figures/analysis/spend_density.pdf")
spend_p <- metric_df %>%
  ggplot() +
  geom_histogram(
    aes(AdSpend, y = ..count.. / sum(..count..)),
    binwidth = quantile(full$AdSpend, 0.95) * 0.05,
    colour = "white",
    size = 1,
    boundary = 0
  ) +
  xlim(0, quantile(full$AdSpend, 0.95)) +
  ylim(0, 0.55) +
  ylab("Proportion") +
  xlab("Spend (RUB)")

spend_p_sub <- full %>%
  filter(AdSpend < quantile(AdSpend, 0.99)) %>%
  ggplot() +
  geom_density(aes(AdSpend, ..count.. / sum(..count..))) +
  ylab("") +
  xlab("")

ggdraw() +
  draw_plot(spend_p + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(spend_p_sub +
              theme(legend.justification = "top"),
            0.5,
            0.5,
            0.5,
            0.5)
dev.off()
