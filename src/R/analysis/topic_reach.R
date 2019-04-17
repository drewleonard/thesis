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

# PLOT _
# Plot grouped topics' clicks, impressions, spend
topic_metrics <- full %>%
  drop_na(primary_topic) %>%
  group_by(primary_topic) %>%
  summarise(
    sum_count = n(),
    sum_clicks = sum(Clicks),
    sum_impressions = sum(Impressions),
    sum_spend = sum(AdSpend)
  ) %>%
  mutate(
    pct_count = sum_count / sum(sum_count),
    pct_clicks = sum_clicks / sum(sum_clicks),
    pct_impressions = sum_impressions / sum(sum_impressions),
    pct_spend = sum_spend / sum(sum_spend)
  ) %>%
  select(primary_topic,
         pct_count,
         pct_clicks,
         pct_impressions,
         pct_spend)

pdf('~/Documents/thesis/data/figures/analysis/topic_reach.pdf')
topic_metrics %>%
  filter(primary_topic != "Mixed" &
           primary_topic != "Music Streaming") %>%
  ggplot(aes(y = reorder(factor(primary_topic), pct_clicks))) +
  geom_point(aes(x = pct_impressions, color = "impressions"), size = 2) +
  geom_point(aes(x = pct_clicks, color = "clicks"), size = 2) +
  scale_colour_manual(
    name = "",
    values = c("clicks" = "#2196F3", "impressions" = "#90CAF9"),
    labels = c("Clicks", "Impressions")
  ) +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = c(0.65, 0.15),
    axis.ticks = element_blank(),
    legend.background = element_rect(size = 0.5, 
                                     linetype="solid", 
                                     colour ="black"),
    legend.title = element_blank()
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()
