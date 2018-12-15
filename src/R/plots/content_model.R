rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(furrr)

# Settings
plan(multiprocess)

# Read data
df <- read.csv('csv/fb_gold.csv')

# Process data
processed <- textProcessor(documents = df$AdText, metadata = df)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

modelContentInterests <- stm(out$documents, out$vocab, K = 49,
                           prevalence =~ Interests,
                           content =~ Interests,
                           interactions = FALSE,
                           data = out$meta, init.type = "Spectral")

# Time and interests interaction
poliblogInteraction4 <- stm(out$documents, out$vocab, K = 49,
                            prevalence =~ AccountGroupCluster,
                            content =~ AccountGroupCluster,
                            interactions = FALSE,
                            data = out$meta, init.type = "Spectral")

plot(poliblogInteraction4, 
     type = "perspectives", 
     n = 30,
     covarlevels = c('cluster_1', 'cluster_2'),
     topics = 21)

plot(poliblogInteraction4, 
     type = "perspectives", 
     n = 30,
     covarlevels = c('cluster_1', 'cluster_2'),
     topics = 30)

# Interests content
modelContentInterests <- readRDS('~/Documents/thesis/data/rds/model_content_interests.RDS')

# Community
plot(modelContentInterests, 
     type = "perspectives", 
     n = 50,
     covarlevels = c('Left Wing', 'Right Wing'),
     topics = 26)

# Social values
plot(modelContentInterests, 
     type = "perspectives", 
     n = 50,
     covarlevels = c('Black America', 'Right Wing'),
     topics = 31)

# Self-defense event GUN TOPIC
plot(modelContentInterests, 
     type = "perspectives", 
     n = 30,
     plabels = c('Black America', 'Second Amendment'),
     covarlevels = c('Black America', 'Second Amendment'),
     topics = 21)

# Heritage
plot(modelContentInterests, 
     type = "perspectives", 
     topics = 29,
     n = 50,
     text.cex = 1.5,
     plabels = c('Left Wing', 'Right Wing'),
     covarlevels = c('Left Wing', 'Right Wing'))

# Age
ageContentEffects <- stm(out$documents, out$vocab, K = 49,
  prevalence =~ Interests,
  content =~ AgeAverageBin,
  interactions = FALSE,
  data = out$meta, init.type = "Spectral")
plot(ageContentEffects, 
     type = "perspectives", 
     n = 30,
     text.cex = 1.5,
     plabels = c('Low', 'High'),
     covarlevels = c('LowAge', 'HighAge'),
     topics = 38)
