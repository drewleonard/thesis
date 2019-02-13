rm(list = ls())
setwd('/Users/drewnleonard/Documents/thesis/')
set.seed(1)

library(tidyverse)
library(readr)
library(quanteda)
library(texteffect)
library(rlang)
library(data.table)

# Read in ocr data
fb_df <- read_csv("data/csv/fb_gold.csv") %>% 
  filter(survey_number != "Unavailable") %>% 
  mutate(survey_number = as.numeric(survey_number))

# Read in survey data
survey_df <- read_csv("data/csv/survey/toy_data_5.csv") %>% 
  slice(3:n())

a<- melt(survey_df, id.vars=c("ResponseId","imgTagsChild.imgTag0","imgTagsChild.imgTag1"), measure.vars= c("q1_0_1","q1_1_1"))

a$img <- NA
a$img[a$variable=="q1_0_1"] <- a$imgTagsChild.imgTag0[a$variable=="q1_0_1"]
a$img[a$variable=="q1_1_1"] <- a$imgTagsChild.imgTag1[a$variable=="q1_1_1"]
  
a$img
# # Join survey and ocr dataframes
# joint_df <- left_join(survey_df, fb_df, by = c("image_id" = "survey_number"))
# 
# # Prepare df for prep into dfm
# # Here, we need AdID, AdText, and response vector
# df_small <- joint_df %>% 
#   dplyr::select(ResponseId, AdID, AdText, Q5_1) %>% 
#   filter(!is.na(AdText))
# 
# # Create corpus object
# # AdID is doc id field, there will be duplicates here
# corpus <- quanteda::corpus(df_small, 
#                            docid_field = "AdID",
#                            text_field = "AdText")
# 
# # Convert to document-feature matrix
# dfm <- quanteda::dfm(corpus)
# 
# # Convert to dataframe
# df <- quanteda::convert(dfm, to = "data.frame")
# 
# # Merge responses into df
# merged_df <- merge(df, 
#                    docvars(dfm), 
#                    by.x = "document", 
#                    by.y = "VALUE")
# 
# # Get treatments and responses
# X <- merged_df %>% 
#   dplyr::select(-c("Q5_1", "document"))
# Y <- merged_df %>% 
#   dplyr::pull(Q5_1)
# Y <- as.numeric(Y)
# 
# # Get training index vector
# train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)
# 
# sibp.fit <- texteffect::sibp(X, Y, K = 2, alpha = 4, sigmasq.n = 0.8,
#                  train.ind = train.ind)
# 
# %>% 
#   gather("image_tag", "image_url", paste("imgTagsChild.imgTag", c(0:9), sep="")) %>% 
#   rowwise() %>% 
#   mutate(image_id = as.numeric(str_extract_all(image_url, "[0-9]+")[[1]]),
#          imgtag_n = as.numeric(str_extract_all(image_tag, "[0-9]+")[[1]]))


