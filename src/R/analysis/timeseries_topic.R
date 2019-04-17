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

pdf('~/Documents/thesis/data/figures/analysis/extra/timeseries_topic.pdf')
full %>%
  drop_na(primary_topic) %>%
  filter(primary_topic != "Mixed" &
           primary_topic != "Music Streaming") %>%
  mutate() %>%
  ggplot(aes(
    x = as.Date(CreationDateFormatted, "%Y-%m-%d"),
    y = as.factor(primary_topic)
  )) +
  geom_density_ridges2(aes(alpha = 0.5)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_alpha(guide = 'none') +
  labs(title = "", x = "", y = "")  
dev.off()
