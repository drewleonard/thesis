rm(list = ls())
setwd('/Users/drewnleonard/Documents/thesis/')
set.seed(1)

library(dplyr)
library(texteffect)
library(quanteda)
library(stm)

# Load in training set 1
train_set_1 <- readr::read_csv("data/csv/survey/split_first_1.csv")

# Filter training set 1
# Remove null AdText rows
# Select response variable
train_set_1_small <- train_set_1 %>% 
  dplyr::select(AdID, AdText, Q5_1) %>% 
  dplyr::filter(!is.na(AdText))

# Create corpus object
# AdID is doc id field, there will be duplicates here
train_set_1_corpus <- quanteda::corpus(train_set_1_small, 
                                       docid_field = "AdID", 
                                       text_field = "AdText")

# Convert to document-feature matrix
train_set_1_dfm <- quanteda::dfm(train_set_1_corpus)

# Convert to dataframe
train_set_1_df <- quanteda::convert(train_set_1_dfm, 
                                    to = "data.frame")

# Merge responses into df
merged_df <- merge(train_set_1_df, 
                   docvars(train_set_1_dfm), 
                   by.x = "document", 
                   by.y = 0)

# Get treatments and responses
X <- merged_df %>% 
  dplyr::select(-c("Q5_1", "document"))
Y <- merged_df %>% 
  dplyr::pull(Q5_1)

# Train sibp model object
sibp.search <- sibp_param_search(X, 
                                 Y,
                                 K = 2,
                                 alphas = c(2,4),
                                 sigmasq.ns = c(0.8, 1),
                                 iters = 1,
                                 train.ind = c(1:nrow(X)))

# Evaluate trained sibp model for hyperparameters
sibp_rank_runs(sibp.search, X, 10)

# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["4"]][["0.8"]][[1]], colnames(X), 10, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["1"]][[1]], colnames(X), 10, verbose = TRUE)

# Select the most interest treatments to investigate
sibp.fit <- sibp.search[["4"]][["0.8"]][[1]]
sibp.fit["test.ind"] <- c(1:nrow(X))
