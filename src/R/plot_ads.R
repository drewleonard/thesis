rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(tidyverse)
library(forestplot)
library(ggridges)
library(formatR)
library(cowplot)
library(ggrepel)

# Load and process data file from search_k_gold.R
df <- read.csv('csv/df_interests_spline_age_49.csv')
stmobj <- readRDS("~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")

