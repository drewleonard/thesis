rm(list = ls())
setwd('~/Documents/thesis/data/')

load('rdata/survey_analysis_partisan_remote.RData')

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
library(ggplot2)
library(cowplot)

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

sibp_amce_temp <- function(sibp.fit,
                           X,
                           Y,
                           G,
                           seed = 0,
                           level = 0.05,
                           thresh = 0.9) {
  # Want it to be the case that G %*% beta selects the correct beta
  if (is.null(G)) {
    G <- matrix(1, nrow = nrow(X), ncol = 1)
  }
  
  set.seed(seed)
  
  G.test <- G[sibp.fit$test.ind, , drop = FALSE]
  Z.test <- infer_Z(sibp.fit, X)
  Y.test <- (Y[sibp.fit$test.ind] - sibp.fit$meanY) / sibp.fit$sdY
  
  Z.hard <-
    apply(Z.test, 2, function(z)
      sapply(z, function(zi)
        ifelse(zi >= 0.9, 1, 0)))
  
  L <- sibp.fit$L
  K <- sibp.fit$K
  
  if (L == 1) {
    fit <- lm(Y.test ~ Z.hard)
  }
  else{
    rhsmat <- c()
    for (l in 1:L) {
      rhsmat <- cbind(rhsmat, Z.hard * G.test[, l])
    }
    fit <- lm(Y.test ~ -1 + as.matrix(G.test) + rhsmat)
  }
  ci.bounds <-
    cbind(
      coef(fit) + qnorm(level / 2) * summary(fit)$coefficients[, 2],
      coef(fit) + qnorm(1 - level / 2) * summary(fit)$coefficients[, 2]
    )
  
  cidf <- data.frame(
    x = 1:((K + 1) * L),
    effect = coef(fit),
    L = ci.bounds[, 1],
    U = ci.bounds[, 2]
  )
  cidf[,-1] <- cidf[,-1] * sibp.fit$sdY
  sibp.amce <- cidf
  return(sibp.amce)
}

draw_treatment_effects <-
  function(sibp.amce,
           levels,
           treatments,
           levels_title,
           effect_title,
           xlim_l,
           xlim_u) {
    subset_start <- length(levels) + 1
    subset_end <- nrow(sibp.amce)
    estimate_df <- sibp.amce[c(subset_start:subset_end),]
    estimate_df$level <-
      rep(levels, each = nrow(estimate_df) / length(levels))
    estimate_df$treatment <-
      rep(treatments, times = nrow(estimate_df) / length(treatments))
    print(estimate_df)
    estimate_df %>%
      ggplot(., aes(
        x = effect,
        y = level,
        xmin = L,
        xmax = U
      )) +
      geom_point() +
      geom_errorbarh(height = .1) +
      facet_grid(. ~ treatment) +
      xlim(xlim_l, xlim_u) +
      labs(y = levels_title, x = effect_title) +
      theme_bw() +
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color = "grey"),
        axis.line = element_line(color = "grey", size = 0.5),
        panel.border = element_rect(
          color = "grey",
          fill = NA,
          size = 0.5
        )
      )
    
  }

# Load survey data
# 3259 responses
df_survey <- read.csv('csv/survey/IRA.csv')

# Load facebook data
df_fb <- read_csv("csv/fb_gold.csv") %>%
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
  # Democrat == 1
  # Republican == 2
  filter(partisanship == 1 | partisanship == 2) %>%
  mutate(# Strong partisan == 1
    # Not strong partisn == 2
    partisanship_strength = ifelse((partisanship == 1 &
                                      partisanship_strength_democrat == 1) |
                                     (partisanship == 2 &
                                        partisanship_strength_republican == 1),
                                   1,
                                   2
    )) %>%
  mutate(
    # img*_q0_1 measures feelings toward Democrats
    # img*_q0_2 measures feelings toward Republicans
    img0_affect_partisanship = ifelse(partisanship == 1, img0_q0_1 - img0_q0_2, img0_q0_2 - img0_q0_1),
    img1_affect_partisanship = ifelse(partisanship == 1, img1_q0_1 - img1_q0_2, img1_q0_2 - img1_q0_1),
    img2_affect_partisanship = ifelse(partisanship == 1, img2_q0_1 - img2_q0_2, img2_q0_2 - img2_q0_1),
    img3_affect_partisanship = ifelse(partisanship == 1, img3_q0_1 - img3_q0_2, img3_q0_2 - img3_q0_1),
    img4_affect_partisanship = ifelse(partisanship == 1, img4_q0_1 - img4_q0_2, img4_q0_2 - img4_q0_1),
    img5_affect_partisanship = ifelse(partisanship == 1, img5_q0_1 - img5_q0_2, img5_q0_2 - img5_q0_1),
    img6_affect_partisanship = ifelse(partisanship == 1, img6_q0_1 - img6_q0_2, img6_q0_2 - img6_q0_1),
    img7_affect_partisanship = ifelse(partisanship == 1, img7_q0_1 - img7_q0_2, img7_q0_2 - img7_q0_1),
    img8_affect_partisanship = ifelse(partisanship == 1, img8_q0_1 - img8_q0_2, img8_q0_2 - img8_q0_1),
    img9_affect_partisanship = ifelse(partisanship == 1, img9_q0_1 - img9_q0_2, img9_q0_2 - img9_q0_1)
  )

# Get df for given response
df_survey_affect_partisanship <- df_survey_small %>%
  gather(
    "img_n",
    "response",
    img0_affect_partisanship,
    img1_affect_partisanship,
    img2_affect_partisanship,
    img3_affect_partisanship,
    img4_affect_partisanship,
    img5_affect_partisanship,
    img6_affect_partisanship,
    img7_affect_partisanship,
    img8_affect_partisanship,
    img9_affect_partisanship
  ) %>% mutate(img_tag = ifelse(
    img_n == "img0_affect_partisanship",
    as.character(imgTagsChild.imgTag0),
    ifelse(
      img_n == "img1_affect_partisanship",
      as.character(imgTagsChild.imgTag1),
      ifelse(
        img_n == "img2_affect_partisanship",
        as.character(imgTagsChild.imgTag2),
        ifelse(
          img_n == "img3_affect_partisanship",
          as.character(imgTagsChild.imgTag3),
          ifelse(
            img_n == "img4_affect_partisanship",
            as.character(imgTagsChild.imgTag4),
            ifelse(
              img_n == "img5_affect_partisanship",
              as.character(imgTagsChild.imgTag5),
              ifelse(
                img_n == "img6_affect_partisanship",
                as.character(imgTagsChild.imgTag6),
                ifelse(
                  img_n == "img7_affect_partisanship",
                  as.character(imgTagsChild.imgTag7),
                  ifelse(
                    img_n == "img8_affect_partisanship",
                    as.character(imgTagsChild.imgTag8),
                    ifelse(
                      img_n == "img9_affect_partisanship",
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
         partisanship,
         partisanship_strength,
         img_key)

# Merge survey and facebook dfs
df_merged <-
  inner_join(df_survey_affect_partisanship,
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
  select(img_key, response, AdText, partisanship, partisanship_strength)

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
dtm_sparse <- tm::removeSparseTerms(dtm, 0.9925)
df_dtm_sparse <-
  as.data.frame(as.matrix(dtm_sparse), stringsAsFactors = False)

# Join df_dfm onto df_merged_small
df_merged_small_dfm <-
  merge(df_merged_small, df_dtm_sparse, by = "row.names")

# Split into training and testing sets
Y <- df_merged_small_dfm %>%
  pull(response)

X <- df_merged_small_dfm %>%
  select(
    -c(
      'Row.names',
      'img_key',
      'response',
      'AdText',
      'partisanship',
      'partisanship_strength'
    )
  )

# Split at specific randomization
set.seed(1)
train.ind <-
  sample(1:nrow(X), size = 0.5 * nrow(X), replace = FALSE)

# Model 1: G for partisanship

# Get Nx2 matrix G for sub-population analysis
df_merged_small_dfm$partisanship[df_merged_small_dfm$partisanship == 2] <-
  0
G <- df_merged_small_dfm %>% select(partisanship)
G$democrat <- G$partisanship
G$republican <- abs(G$partisanship - 1)
G <- G %>% select(democrat, republican)
G <- as.matrix(G)

# Search across params for 3 treatments
sibp.search_3 <-
  texteffect::sibp_param_search(
    X,
    Y,
    K = 3,
    alphas = c(3, 4, 5),
    sigmasq.ns = c(0.50, 0.75, 1.00),
    iters = 3,
    train.ind = train.ind,
    G = G,
    seed = 0
  )

sibp.rank <- sibp_rank_runs(sibp.search_3, X, 30)

sibp.fit <- sibp.search_3[["3"]][["0.75"]][[1]]
sibp_top_words(sibp.fit, colnames(X), 30, verbose = TRUE)

sibp.amce <- sibp_amce_temp(sibp.fit, X, Y, G = G)
sibp_amce_plot(sibp.amce, L = 2)

draw_treatment_effects(
  sibp.amce = sibp.amce,
  levels = c("Democrats", "Republicans"),
  treatments = c("Dangerous Society", "Identity Support", "Black Pride"),
  levels_title = "Partisanship",
  effect_title = "Partisan Affective Polarization",
  xlim_l = -20,
  xlim_u = 20
)

e# # Method for viewing interventions with treatment
# r <- df_merged_small_dfm %>%
#   rename(foo_bar = day) %>%
#   filter(foo_bar != 0) %>%
#   select(foo_bar, AdText) %>%
#   arrange(desc(foo_bar))
# View(r)

save.image('./rdata/survey_analysis_partisan.RData')
