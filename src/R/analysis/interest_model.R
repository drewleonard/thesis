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
df_small <- df %>% 
  select('AdID', 'Interests') %>% 
  filter(!is.na(Interests) & grepl("^\\s*$", Interests) == FALSE)

# Preprocess texts
processed <- textProcessor(documents = df_small$Interests, metadata = df_small)
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

stmobj <- stm(out$documents, 
              out$vocab, 
              K = 20,
              data = out$meta,
              init.type = "Spectral")
saveRDS(stmobj, '~/Documents/thesis/data/rds/interests_stm_20.RDS')

pdf("~/Documents/thesis/data/figures/appendix/interests_stm_20.pdf")
plot(stmobj,n=10)
dev.off()

# Get and cast theta object from STM
theta <- stmobj$theta
thetadf <- as.data.frame(theta)

# Attach topic names to thetadf
topicNames <- c('T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12', 'T13', 'T14', 'T15', 'T16', 'T17', 'T18', 'T19', 'T20')
names(thetadf) <- topicNames

# Attach AdID field to topics
AdID <- out$meta[,"AdID"]
topics <- as.data.frame(cbind(thetadf,AdID))

# Get primary topic for each ad
topics$"primary_topic" <- colnames(topics[,c(1:ncol(stmobj$theta))])[max.col(topics[,c(1:ncol(stmobj$theta))],ties.method="first")]

# Drop topic loadings
topics <- topics[,c('AdID','primary_topic')]

# Create df with full ad data + topic loadings
full <- left_join(df, topics,  by=c("AdID" = "AdID"))
