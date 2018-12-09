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

poliblogInteraction3 <- stm(out$documents, out$vocab, K = 49,
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

poliblogInteraction3 <- readRDS('~/Documents/thesis/data/rds/model_content_interests.RDS')


# Community
plot(poliblogInteraction3, 
     type = "perspectives", 
     n = 37,
     covarlevels = c('Left Wing', 'Right Wing'),
     topics = 26)

# Social values
plot(poliblogInteraction3, 
     type = "perspectives", 
     n = 50,
     covarlevels = c('Black America', 'Right Wing'),
     topics = 31)

# Self-defense event GUN TOPIC
plot(poliblogInteraction3, 
     type = "perspectives", 
     n = 10,
     plabels = c('Self Defense', 'Second Amendment'),
     covarlevels = c('Self Defense', 'Second Amendment'),
     topics = 21)

# Heritage
plot(poliblogInteraction3, 
     type = "perspectives", 
     topics = 29,
     n = 40,
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
