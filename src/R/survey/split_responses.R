rm(list = ls())
setwd('/Users/drewnleonard/Documents/thesis/')
set.seed(123)

library(tidyverse)
library(readr)

# Read in ocr data
fb_df <- read_csv("data/csv/fb_gold.csv") %>% 
  filter(survey_number != "Unavailable") %>% 
  mutate(survey_number = as.numeric(survey_number))

# Read in survey data
image_tags <- paste("imgTagsChild.imgTag", c(0:9), sep="")
survey_df <- read_csv("data/csv/survey/dummy_responses.csv") %>% 
  slice(3:3) %>% 
  gather("image_tag", "image_url", image_tags) %>% 
  group_by(image_url) %>% 
  mutate(image_id = as.numeric(str_extract_all(image_url, "[0-9]+")[[1]])) %>% 
  ungroup()

# Join survey and ocr dataframes
joint_df <- left_join(survey_df, fb_df, by = c("image_id" = "survey_number"))

# Split joint df
split_size <- floor(0.50 * nrow(joint_df))
split_vector <- sample(seq_len(nrow(joint_df)), split_size)
split_first <- joint_df[split_vector, ]
split_second <- joint_df[-split_vector, ]

# Download splits
write_csv(split_first, path = "data/csv/survey/split_first_1.csv")
write_csv(split_second, path = "data/csv/survey/split_second_1.csv")

# Get missing indexed survey numbers
survey_col <- fb_df$survey_number
for (index in seq(1:3072)) {
  if (!(index %in% survey_col)) {
    print(index)
  }
}

