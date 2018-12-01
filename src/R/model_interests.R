rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load libraries
library(furrr)
library(dplyr)
library(stm)
library(cowplot)

# Settings
plan(multiprocess)

# Read CSV
df <- read.csv('csv/FacebookAds.csv')

# Subset df
df <- df %>% 
  select('AdID', 'Interests') %>% 
  filter(!is.na(Interests) & grepl("^\\s*$", Interests) == FALSE)

# Preprocess texts
processed <- textProcessor(documents = df$Interests, metadata = df)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# searchK_results <- searchK(out$documents, 
#                            out$vocab, 
#                            K = c(10,15,20,25,30),
#                            data = out$meta,
#                            init.type = "Spectral")
# saveRDS(searchK_results, "~/Documents/thesis/data/rds/interests_searchk_10_30_5.RDS")

searchK_results <- readRDS("~/Documents/thesis/data/rds/interests_searchk_10_30_5.RDS")
pdf("~/Documents/thesis/data/figures/appendix/interests_searchk_10_30_5.pdf")
plot(searchK_results)
dev.off()
