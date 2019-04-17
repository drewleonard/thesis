rm(list = ls())
setwd('~/Documents/thesis/data/')

load('./rdata/survey_analysis_race.RData')
source("../src/R/survey/survey_analysis_helper.R")

library(rlang)
library(data.table)
library(texteffect)
library(tidyr)
library(dplyr)
library(readr)
library(tm)
library(textstem)
library(stringr)
library(tokenizers)

# Load survey data
# 3259 responses
df_survey <- read.csv('data/IRA.csv')

# Load facebook data
df_fb <- read_csv("data/fb_gold.csv") %>%
  filter(survey_number != "Unavailable") %>%
  filter(!grepl('musicfb', AdText)) %>%
  filter(!grepl('facemusic', AdText)) %>%
  filter(!grepl('music', AdText)) %>%
  filter(!grepl('browser', AdText)) %>%
  filter(!grepl('download', AdText)) %>%
  filter(!grepl('online_player', AdText)) %>%
  filter(!grepl('safe with us', AdText)) %>%
  filter(!grepl('bring your friends', AdText)) %>%
  filter(!grepl('self-defense', AdText)) %>%
  mutate(survey_number = as.numeric(survey_number))

# Get small survey frame
df_survey_small <- df_survey %>%
  select(
    StartDate,
    ResponseId,
    Status,
    Finished,
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
  dplyr::rename(
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
  # White == 1
  # Black == 2
  filter(race == 1 | race == 2) %>%
  # Democrat == 1
  # Republican == 2
  filter(partisanship == 1 | partisanship == 2) %>%
  # img*_q0_4 measures feelings toward whites
  # img*_q0_5 measures feelings toward blacks
  mutate(
    img0_affect_race = ifelse(race == 1, img0_q0_4 - img0_q0_5, img0_q0_5 - img0_q0_4),
    img1_affect_race = ifelse(race == 1, img1_q0_4 - img1_q0_5, img1_q0_5 - img1_q0_4),
    img2_affect_race = ifelse(race == 1, img2_q0_4 - img2_q0_5, img2_q0_5 - img2_q0_4),
    img3_affect_race = ifelse(race == 1, img3_q0_4 - img3_q0_5, img3_q0_5 - img3_q0_4),
    img4_affect_race = ifelse(race == 1, img4_q0_4 - img4_q0_5, img4_q0_5 - img4_q0_4),
    img5_affect_race = ifelse(race == 1, img5_q0_4 - img5_q0_5, img5_q0_5 - img5_q0_4),
    img6_affect_race = ifelse(race == 1, img6_q0_4 - img6_q0_5, img6_q0_5 - img6_q0_4),
    img7_affect_race = ifelse(race == 1, img7_q0_4 - img7_q0_5, img7_q0_5 - img7_q0_4),
    img8_affect_race = ifelse(race == 1, img8_q0_4 - img8_q0_5, img8_q0_5 - img8_q0_4),
    img9_affect_race = ifelse(race == 1, img9_q0_4 - img9_q0_5, img9_q0_5 - img9_q0_4)
  )

# Get df for given response
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
  rowwise() %>%
  filter(!is.na(response)) %>%
  mutate(img_key = get_image_n(img_tag)) %>%
  select(ResponseId,
         response,
         race,
         partisanship,
         img_key)

# Merge survey and facebook dfs
df_merged <-
  inner_join(df_survey_affect_race,
             df_fb,
             by = c("img_key" = "survey_number")) %>%
  filter(!is.na(AdText))

# Select just responses and texts
df_merged_small <- df_merged %>%
  mutate(
    bigrams = tokenize_ngrams(
      AdText,
      n = 3,
      lowercase = FALSE,
      ngram_delim = "_",
      stopwords = tm::stopwords(kind = "en")
    ),
    bigrams_collapsed = paste(bigrams, collapse = ' ')
  ) %>%
  unite(united, AdText, bigrams_collapsed, sep = " ") %>%
  rename(AdText = united) %>%
  select(img_key, response, AdText, race, partisanship)

# Covert to corpus
corpus <-
  tm::VCorpus(tm::VectorSource(df_merged_small$AdText),
              readerControl = list(language = "en"))

# Strip whitespace
corpus <- tm::tm_map(corpus, tm::stripWhitespace)

# Make lowercase
corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))

# Remove punctuation except pound symbol and apostropher
corpus <-
  tm::tm_map(corpus,
             tm::content_transformer(remove_punctiation),
             preserve_intra_word_dashes = TRUE)

# Remove stop words
corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("en"))

# Remove numbers
corpus <- tm::tm_map(corpus, tm::removeNumbers)

# Lemmatize
corpus <-
  tm_map(corpus,
         tm::content_transformer(textstem::lemmatize_strings))
corpus <-
  tm_map(corpus, tm::content_transformer(collapse_punctuation))

# Convert corpus to dtm
dtm <-
  tm::DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))

# Make dtm sparse
dtm_sparse <- tm::removeSparseTerms(dtm, 0.99375)
df_dtm_sparse <-
  as.data.frame(as.matrix(dtm_sparse), stringsAsFactors = False)

# Join df_dfm onto df_merged_small
df_merged_small_dfm <-
  merge(df_merged_small, df_dtm_sparse, by = "row.names")

# Split into training and testing sets
Y <- df_merged_small_dfm %>%
  pull(response)

X <- df_merged_small_dfm %>%
  select(-c(
    'Row.names',
    'img_key',
    'response',
    'AdText',
    'race.x',
    'partisanship'
  ))

# Split at specific randomization
set.seed(1)
train.ind <-
  sample(1:nrow(X), size = 0.5 * nrow(X), replace = FALSE)

# Get Nx4 matrix G
G <- df_merged_small %>%
  mutate(
    black_democrat = ifelse(race == 2 & partisanship == 1, 1, 0),
    white_democrat = ifelse(race == 1 &
                              partisanship == 1, 1, 0),
    black_republican = ifelse(race == 2 &
                                partisanship == 2, 1, 0),
    white_republican = ifelse(race == 1 &
                                partisanship == 2, 1, 0)
  ) %>%
  select(black_democrat,
         white_democrat,
         black_republican,
         white_republican)
G <- as.matrix(G)

# Search across parameters
sibp.search <-
  texteffect::sibp_param_search(
    X,
    Y,
    K = 3,
    alphas = c(3, 4),
    sigmasq.ns = c(0.50, 0.75, 1.00),
    iters = 10,
    train.ind = train.ind,
    G = G,
    seed = 0
  )

sibp.rank <- sibp_rank_runs(sibp.search, X, 30)

# Finalized
sibp.fit <- sibp.search[["3"]][["0.75"]][[10]]
sibp_top_words(sibp.fit, colnames(X), 30, verbose = TRUE)
sibp.amce <- sibp_amce_temp(sibp.fit, X, Y, G = G)
sibp.amce.model <- get_amce_model(sibp.fit, X, Y, G = G)

#pdf('./figures/survey_analysis_race_effects.pdf')
draw_treatment_effects(
  sibp.amce = sibp.amce,
  treatments = c("Identity Support", "Dangerous Society", "Black Pride"),
  groups_title = "",
  effect_title = "Racial Affective Polarization",
  xlim_l = -50,
  xlim_u = 50
)
#dev.off()

format_treatment_effects(
  sibp.amce = sibp.amce,
  treatments = c("Identity Support", "Dangerous Society", "Black Pride")
)

# Power analysis
get_power_analysis_linear(sibp.amce.model, G)

#save.image('./rdata/survey_analysis_race.RData')
