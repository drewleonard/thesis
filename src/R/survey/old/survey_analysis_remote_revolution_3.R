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

# Function for getting image n from tag
get_image_n <- function(image_tag) {
  as.numeric(str_extract_all(image_tag, "[0-9]+")[[1]])
}

# Function for removing punctuation besides hashtag
remove_punctiation_helper <- function(x) {
  x <- gsub("#", "\002", x)
  x <- gsub("_", "\003", x)
  x <- gsub("[[:punct:]]+", "", x)
  x <- gsub("\002", "#", x, fixed = TRUE)
  gsub("\003", "_", x, fixed = TRUE)
}
remove_punctiation <-
  function (x, preserve_intra_word_dashes = FALSE) {
    if (preserve_intra_word_dashes) {
      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
      x <- remove_punctiation_helper(x)
      gsub("\001", "-", x, fixed = TRUE)
    } else {
      remove_punctiation_helper(x)
    }
  }

# Function to collapse hashtags to lemmas
collapse_punctuation <- function (x) {
  x <- gsub("# ", "#", x, fixed = TRUE)
  gsub(" _ ", "_", x, fixed = TRUE)
}

# Function to remove common terms
remove_common_terms <- function (x, pct) {
  x[, slam::col_sums(x) / nrow(x) <= pct]
}

# Load survey data
# 3259 responses
df_survey <- read.csv('data/IRA.csv')

# Load facebook data
df_fb <- read_csv("data/fb_gold.csv") %>%
  filter(survey_number != "Unavailable") %>%
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
  filter(race == 1 | race == 2) %>% 
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
  filter(!is.na(response)) %>%
  rowwise() %>%
  mutate(img_key = get_image_n(img_tag)) %>%
  select(ResponseId,
         response,
         img_key)

# Merge survey and facebook dfs
df_merged <-
  left_join(df_survey_affect_race, df_fb, by = c("img_key" = "survey_number"))

# Select just responses and texts
df_merged_small <- df_merged %>%
  mutate(
    bigrams = tokenize_ngrams(
      AdText,
      n = 2,
      lowercase = FALSE,
      ngram_delim = "_",
      stopwords = tm::stopwords(kind = "en")
    ),
    bigrams_collapsed = paste(bigrams, collapse = ' ')
  ) %>%
  unite(united, AdText, bigrams_collapsed, sep = " ") %>%
  rename(AdText = united) %>%
  select(img_key, response, AdText)

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
dtm_sparse <- tm::removeSparseTerms(dtm, 0.997)
dtm_sparse <- remove_common_terms(dtm_sparse, 0.10)

# dtm_sparse <- dtm
df_dtm_sparse <-
  as.data.frame(as.matrix(dtm_sparse), stringsAsFactors = False)

# Join df_dfm onto df_merged_small
df_merged_small_dfm <-
  merge(df_merged_small, df_dtm_sparse, by = "row.names")

# Split into training and testing sets
Y <- df_merged_small_dfm %>%
  pull(response)

X <- df_merged_small_dfm %>%
  select(-c('Row.names', 'img_key', 'response', 'AdText'))

print(dim(X))

# Split at specific randomization
set.seed(1)
train.ind <-
  sample(1:nrow(X), size = 0.5 * nrow(X), replace = FALSE)

sibp.search_4_grams_multi_3 <-
  texteffect::sibp_param_search(
    X,
    Y,
    K = 4,
    alphas = c(2,3,4),
    sigmasq.ns = c(0.50,0.75,1.00),
    iters = 2,
    train.ind = train.ind
  )
saveRDS(sibp.search_4_grams_multi_3,
        'sibp.search_4_grams_multi_3.RDS')

sibp.search_5_grams_multi_3 <-
  texteffect::sibp_param_search(
    X,
    Y,
    K = 5,
    alphas = c(2,3,4),
    sigmasq.ns = c(0.50,0.75,1.00),
    iters = 2,
    train.ind = train.ind
  )
saveRDS(sibp.search_5_grams_multi_3,
        'sibp.search_5_grams_multi_3.RDS')

sibp.search_6_grams_multi_3 <-
  texteffect::sibp_param_search(
    X,
    Y,
    K = 6,
    alphas = c(2,3,4),
    sigmasq.ns = c(0.50,0.75,1.00),
    iters = 2,
    train.ind = train.ind
  )
saveRDS(sibp.search_6_grams_multi_3,
        'sibp.search_6_grams_multi_3.RDS')

save.image(file='survey_analysis_remote_revolution_3.RData')
