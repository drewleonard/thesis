rm(list = ls())
setwd('/Users/drewnleonard/Documents/thesis/')
set.seed(1)

library(tidyverse)
library(readr)
library(quanteda)
library(texteffect)

# Read in ocr data
fb_df <- read_csv("data/csv/fb_gold.csv") %>% 
  filter(survey_number != "Unavailable") %>% 
  mutate(survey_number = as.numeric(survey_number))

# Read in survey data
image_tags <- paste("imgTagsChild.imgTag", c(0:9), sep="")
survey_df <- read_csv("data/csv/survey/dummy_responses.csv") %>% 
  gather("image_tag", "image_url", image_tags) %>% 
  group_by(image_url) %>% 
  mutate(image_id = as.numeric(str_extract_all(image_url, "[0-9]+")[[1]])) %>% 
  ungroup()

# Join survey and ocr dataframes
joint_df <- left_join(survey_df, fb_df, by = c("image_id" = "survey_number"))

# Prepare df for prep into dfm
# Here, we need AdID, AdText, and response vector
df_small <- joint_df %>% 
  dplyr::select(ResponseId, AdID, AdText, Q5_1) %>% 
  filter(!is.na(AdText))

# Create corpus object
# AdID is doc id field, there will be duplicates here
corpus <- quanteda::corpus(df_small, 
                           docid_field = "AdID",
                           text_field = "AdText")

# Convert to document-feature matrix
dfm <- quanteda::dfm(corpus)

# Convert to dataframe
df <- quanteda::convert(dfm, to = "data.frame")

# Merge responses into df
merged_df <- merge(df, 
                   docvars(dfm), 
                   by.x = "document", 
                   by.y = "VALUE")

# Get treatments and responses
X <- merged_df %>% 
  dplyr::select(-c("Q5_1", "document"))
Y <- merged_df %>% 
  dplyr::pull(Q5_1)
Y <- as.numeric(Y)

# Get training index vector
train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)

sibp.fit <- texteffect::sibp(X, Y, K = 2, alpha = 4, sigmasq.n = 0.8,
                 train.ind = train.ind)
