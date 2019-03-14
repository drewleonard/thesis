rm(list = ls())
setwd("~/Documents/thesis/data/")

library(rlang)
library(data.table)
library(texteffect)
library(tidyverse)
library(tm)

# Function for getting image n from tag
get_image_n <- function(image_tag) {
  as.numeric(str_extract_all(image_tag, "[0-9]+")[[1]])
}

# Load survey data
# 3259 responses
df_survey <- read.csv('csv/survey/IRA.csv')

# Load facebook data
df_fb <- read_csv("csv/fb_gold.csv") %>%
  filter(survey_number != "Unavailable") %>%
  mutate(survey_number = as.numeric(survey_number))

# Get small survey frame
df_survey_small <- df_survey %>%
  select(
    StartDate,
    ResponseId,
    Status,
    Finished,
    Duration..in.seconds.,
    Q4,
    Q5,
    Q6,
    Q7,
    Q8,
    Q9,
    Q10,
    Q11,
    Q12,
    Q13,
    Q203,
    imgTagsChild.imgTag0,
    img0_q0_1,
    img0_q0_2,
    img0_q0_4,
    img0_q0_5,
    img0_q0_6,
    img0_q1,
    img0_q2,
    imgTagsChild.imgTag1,
    img1_q0_1,
    img1_q0_2,
    img1_q0_4,
    img1_q0_5,
    img1_q0_6,
    img1_q1,
    img1_q2,
    imgTagsChild.imgTag2,
    img2_q0_1,
    img2_q0_2,
    img2_q0_4,
    img2_q0_5,
    img2_q0_6,
    img2_q1,
    img2_q2,
    imgTagsChild.imgTag3,
    img3_q0_1,
    img3_q0_2,
    img3_q0_4,
    img3_q0_5,
    img3_q0_6,
    img3_q1,
    img3_q2,
    imgTagsChild.imgTag4,
    img4_q0_1,
    img4_q0_2,
    img4_q0_4,
    img4_q0_5,
    img4_q0_6,
    img4_q1,
    img4_q2,
    imgTagsChild.imgTag5,
    img5_q0_1,
    img5_q0_2,
    img5_q0_4,
    img5_q0_5,
    img5_q0_6,
    img5_q1,
    img5_q2,
    imgTagsChild.imgTag6,
    img6_q0_1,
    img6_q0_2,
    img6_q0_4,
    img6_q0_5,
    img6_q0_6,
    img6_q1,
    img6_q2,
    imgTagsChild.imgTag7,
    img7_q0_1,
    img7_q0_2,
    img7_q0_4,
    img7_q0_5,
    img7_q0_6,
    img7_q1,
    img7_q2,
    imgTagsChild.imgTag8,
    img8_q0_1,
    img8_q0_2,
    img8_q0_4,
    img8_q0_5,
    img8_q0_6,
    img8_q1,
    img8_q2,
    imgTagsChild.imgTag9,
    img9_q0_1,
    img9_q0_2,
    img9_q0_4,
    img9_q0_5,
    img9_q0_6,
    img9_q1,
    img9_q2
  ) %>%
  rename(
    gender = Q4,
    race = Q5,
    income = Q6,
    education = Q7,
    ideology = Q8,
    partisanship = Q9,
    partisanship_closer = Q10,
    partisanship_strength_republican = Q11,
    partisanship_strength_democrat = Q12,
    age = Q13,
    class = Q203
  ) %>%
  mutate(
    img0_affect_partisan = img0_q0_2 - img0_q0_1,
    img0_affect_race = img0_q0_5 - img0_q0_4,
    img1_affect_partisan = img1_q0_2 - img1_q0_1,
    img1_affect_race = img1_q0_5 - img1_q0_4,
    img2_affect_partisan = img2_q0_2 - img2_q0_1,
    img2_affect_race = img2_q0_5 - img2_q0_4,
    img3_affect_partisan = img3_q0_2 - img3_q0_1,
    img3_affect_race = img3_q0_5 - img3_q0_4,
    img4_affect_partisan = img4_q0_2 - img4_q0_1,
    img4_affect_race = img4_q0_5 - img4_q0_4,
    img5_affect_partisan = img5_q0_2 - img5_q0_1,
    img5_affect_race = img5_q0_5 - img5_q0_4,
    img6_affect_partisan = img6_q0_2 - img6_q0_1,
    img6_affect_race = img6_q0_5 - img6_q0_4,
    img7_affect_partisan = img7_q0_2 - img7_q0_1,
    img7_affect_race = img7_q0_5 - img7_q0_4,
    img8_affect_partisan = img8_q0_2 - img8_q0_1,
    img8_affect_race = img8_q0_5 - img8_q0_4,
    img9_affect_partisan = img9_q0_2 - img9_q0_1,
    img9_affect_race = img9_q0_5 - img9_q0_4
  )

df_survey_affect_race <- df_survey_small %>%
  gather(
    "img_n",
    "response",
    img0_affect_race,
    img1_affect_race,
    img2_affect_race,
    img3_affect_race,
    img4_affect_race,
    img5_affect_race,
    img6_affect_race,
    img7_affect_race,
    img8_affect_race,
    img9_affect_race
  ) %>% mutate(img_tag = ifelse(
    img_n == "img0_affect_race",
    as.character(imgTagsChild.imgTag0),
    ifelse(
      img_n == "img1_affect_race",
      as.character(imgTagsChild.imgTag1),
      ifelse(
        img_n == "img2_affect_race",
        as.character(imgTagsChild.imgTag2),
        ifelse(
          img_n == "img3_affect_race",
          as.character(imgTagsChild.imgTag3),
          ifelse(
            img_n == "img4_affect_race",
            as.character(imgTagsChild.imgTag4),
            ifelse(
              img_n == "img5_affect_race",
              as.character(imgTagsChild.imgTag5),
              ifelse(
                img_n == "img6_affect_race",
                as.character(imgTagsChild.imgTag6),
                ifelse(
                  img_n == "img7_affect_race",
                  as.character(imgTagsChild.imgTag7),
                  ifelse(
                    img_n == "img8_affect_race",
                    as.character(imgTagsChild.imgTag8),
                    ifelse(
                      img_n == "img9_affect_race",
                      as.character(imgTagsChild.imgTag9),
                      NA
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )) %>%
  filter(!is.na(response)) %>%
  rowwise() %>%
  mutate(img_key = get_image_n(img_tag)) %>%
  select(ResponseId,
         response,
         img_key)

# Merge dfs
df_merged <-
  left_join(df_survey_affect_race, df_fb, by = c("img_key" = "survey_number"))

# Select just responses and texts
df_merged_small <- df_merged %>%
  select(img_key, response, AdText)

# Covert to and pre-process corpus
corpus <- tm::Corpus(VectorSource(df_merged_small$AdText))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# Convert corpus to dtm
dtm <- DocumentTermMatrix(corpus)

# Make dtm sparse and convert to df
# dtm_sparse <- removeSparseTerms(dtm, 0.98)
dtm_sparse <- dtm
df_dtm_sparse <- as.data.frame(as.matrix(dtm_sparse), stringsAsFactors=False)

# Join df_dfm onto df_merged_small
df_merged_small_dfm <- 
  merge(df_merged_small, df_dtm_sparse, by = "row.names")

# Split into training and testing sets
Y <- df_merged_small_dfm %>% 
  pull(response)

X <- df_merged_small_dfm %>% 
  select(-c('Row.names','img_key','response','AdText'))

# Split at specific randomization
set.seed(1)
train.ind <-
  sample(1:nrow(X), size = 0.5 * nrow(X), replace = FALSE)

# Search for K
sibp.search_15_5_75 <- sibp(X, Y, K = 15, alpha = 3, sigmasq.n = 0.75, train.ind = train.ind)
saveRDS(sibp.search_15_5_75, 'rds/sibp.search_15_5_75.RDS')
sibp_top_words(sibp.search_15_5_75, colnames(X), 10, verbose = TRUE)
