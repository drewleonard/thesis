rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(tidyverse)
library(ggridges)
library(cowplot)

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

# Density plot, all topics
pdf("~/Documents/thesis/data/figures/topics_date.pdf")
full %>%
  drop_na(primary_topic) %>%
  mutate(primary_topic = factor(primary_topic, levels=rev(levels(primary_topic)))) %>% 
  ggplot(aes(x = as.Date(CreationDateFormatted, "%Y-%m-%d"), y = primary_topic)) +
  geom_density_ridges2(aes(alpha = 0.95)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(title = "", x = "", y = "") + 
  theme(legend.position="none") +
dev.off()

# Estimate effects topic-wise
stm_cluster <- readRDS('./rds/model_content_cluster.RDS')
prep_cluster <- estimateEffect(c(1:49) ~ s(CreationDateInteger,5) * AccountGroupCluster, stm_cluster,
                       metadata = out$meta, uncertainty = "None")

pdf('~/Documents/thesis/data/figures/timeseries_bearing_arms.pdf',width=10,height=5)
plot(prep_cluster,
     topics = c(17,21,46),
     covariate = "CreationDateInteger",
     model = stm,
     xaxt = "n",
     yaxt = "n",
     bty='l',
     text.cex = 1.25,
     method = "continuous",
     moderator = "AccountGroupCluster",
     labeltype = "custom",
     custom.labels = c('Police Brutality', 'Bearing Arms', 'Black Empowerment'),
     moderator.value = 'cluster_1')
axis(1, at=c(20150000,20155000,20160000,20165000,20170000), 
     labels = c('01/15','06/15','01/16','06/16','01/17'),
     tck = -0.00)
axis(2, at=c(-0.5,0.0,0.2,0.4,0.6,0.8), 
     tck = -0.00)
dev.off()
