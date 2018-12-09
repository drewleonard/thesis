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