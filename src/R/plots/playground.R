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

# Time and interests interaction
poliblogInteraction3 <- stm(out$documents, out$vocab, K = 49,
                           prevalence =~ Interests,
                           content =~ Interests,
                           interactions = FALSE,
                           data = out$meta, init.type = "Spectral")

saveRDS(poliblogInteraction3, '~/Documents/thesis/data/rds/model_content_interests.RDS')

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
     n = 50,
     covarlevels = c('Self Defense', 'Second Amendment'),
     topics = 21)

plot(poliblogInteraction3, 
     type = "perspectives", 
     topics = 29,
     n = 40,
     covarlevels = c('Left Wing', 'Right Wing'))

