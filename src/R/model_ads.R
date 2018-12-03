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

# stmobj <- stm(out$documents,
#               out$vocab,
#               K = 49,
#               prevalence =~ Interests + s(AgeAverage),
#               data = out$meta,
#               init.type = "Spectral")
#saveRDS(stmobj, "~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")
stmobj <- readRDS("~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")

theta <- stmobj$theta
thetadf <- as.data.frame(theta)

# Name thetadf columns, topic-wise
topicNames <- c("Race Tensions","Black Empowerment","Black Empowerment","Black Empowerment","Incarceration","Communal Support","Race Tensions","Race Tensions","Mixed","Islam in America","Race Tensions","Black Empowerment","Black Empowerment","Mixed","Mixed","Police Brutality","Police Brutality","Race Tensions","Police Brutality","Patriotism","Bearing Arms","Social Justice","Mixed","Mixed","Music Streaming","Patriotism","Black Empowerment","Black Empowerment","Race Tensions","Mixed","Social Justice","Black Empowerment","Black Empowerment","National Security","Police Brutality","Race Tensions","Election","Patriotism","Black Empowerment","Social Justice","Minorities","Race Tensions","Police Brutality","Mixed","Race Tensions","Black Empowerment","Black Empowerment","Minorities","Black Empowerment")
names(thetadf) <- topicNames

# Attach AdID field to topics
AdID <- out$meta[,"AdID"]
topics <- as.data.frame(cbind(thetadf,AdID))

# Get primary topic for each ad
topics$"primary_topic" <- colnames(topics[,c(1:ncol(stmobj$theta))])[max.col(topics[,c(1:ncol(stmobj$theta))],ties.method="first")]

# Clean primary topic labels
topics$"primary_topic" <- gsub('[[:digit:]]+', '', topics$"primary_topic")
topics$"primary_topic" <- gsub('[.]', '', topics$"primary_topic")

# Drop topic loadings
topics <- topics[,c('AdID','primary_topic')]

# Create df with full ad data + topic loadings
full <- left_join(df, topics,  by=c("AdID" = "AdID"))

write_csv(full, "~/Documents/thesis/data/csv/df_interests_spline_age_49.csv")
