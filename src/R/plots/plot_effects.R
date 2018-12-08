rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(tidyverse)
library(forestplot)
library(formatR)
library(cowplot)

# Helper function for making forest plot
formatInterval <- function(mean, lower, upper) {
  upper <- formatC(upper, format = "f", digits = 3)
  upper <- ifelse(grepl("-", upper), str_c("", upper), str_c("  ", upper))
  lower <- formatC(lower, format = "f", digits = 3)
  lower <- ifelse(grepl("-", lower), str_c(" ", lower), str_c("   ", lower))
  interval <- str_c("[", lower, " , ", upper, " ]")
  mean <- formatC(mean, format = "f", digits = 3)
  return(str_c(mean, "   ", interval))
}

# Function for making forest plots
makePrimaryTopicForestPlot <- function(stm, file_name, prep, covar_name, left_val, right_val, left_lab, right_lab) {
  plot <- plot(prep,
    covariate = covar_name,
    topics = c(1:49),
    model = stm,
    method = "difference",
    cov.value1 = right_val,
    cov.value2 = left_val,
    xlab = "",
    main = "",
    labeltype = "custom",
    custom.labels = topicNames
  )

  plotDF <- data.frame(t(sapply(plot$cis, function(x) x[1:max(lengths(plot$cis))])))

  prepDF <- data.frame(
    plot$labels,
    plot$topics,
    unlist(plot$means),
    plotDF$X2.5.,
    plotDF$X97.5.
  )

  colnames(prepDF) <- c(
    "primary_topic",
    "topic_n",
    "mean",
    "lower",
    "upper"
  )

  prepDF <- prepDF %>%
    group_by(primary_topic) %>%
    summarise(
      avg_ci_point_estimate = mean(mean),
      avg_ci_lower_bound = mean(lower),
      avg_ci_upper_bound = mean(upper)
    )

  table_text <- cbind(
    c(NA, seq(1, length(prepDF$primary_topic))),
    c("Primary Topic", as.vector(prepDF$primary_topic)),
    c("Confidence Interval (95%)", formatInterval(
      prepDF$avg_ci_point_estimate,
      prepDF$avg_ci_lower_bound,
      prepDF$avg_ci_upper_bound
    ))
  )

  pdf(str_c("~/Documents/", file_name))
  forestplot(table_text,
    graph.pos = 3,
    is.summary = c(TRUE, rep(FALSE, length(prepDF$primary_topic))),
    align = c("r", "l", "r"),
    mean = c(NA, prepDF$avg_ci_point_estimate),
    lower = c(NA, prepDF$avg_ci_lower_bound),
    upper = c(NA, prepDF$avg_ci_upper_bound),
    fn.ci_norm = fpDrawCircleCI,
    hrzl_lines = list("2" = gpar(lwd = 2, col = "#000000")),
    boxsize = .15,
    xlab = (str_c("\n", left_lab, " ... ", right_lab)),
    cex = 0,
    zero = 0,
    new_page = FALSE,
    txt_gp = fpTxtGp(
      xlab = gpar(cex = .75),
      label = gpar(cex = .75),
      ticks = gpar(cex = .75)
    ),
    col = fpColors(box = "black", lines = "black", zero = "gray50"),
    cex = 0.9, lineheight = "auto", colgap = unit(4, "mm"),
    lwd.ci = 1,
    clip = c(-.1, 1)
  )
  dev.off()
}

# Read data
fb <- read.csv("csv/fb_gold.csv")

# Process data
processed <- textProcessor(documents = fb$AdText, metadata = fb)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Load full, stm
full <- read.csv("csv/df_interests_spline_age_49.csv")
stm <- readRDS("~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")

# Set topic names
topicNames <- c(
  "Race Tensions", "Black Empowerment", "Black Empowerment",
  "Black Empowerment", "Incarceration", "Communal Support",
  "Race Tensions", "Race Tensions", "Mixed",
  "Islam in America", "Race Tensions", "Black Empowerment",
  "Black Empowerment", "Mixed", "Mixed",
  "Police Brutality", "Police Brutality", "Race Tensions",
  "Police Brutality", "Patriotism", "Bearing Arms",
  "Social Justice", "Mixed", "Mixed",
  "Music Streaming", "Patriotism", "Black Empowerment",
  "Black Empowerment", "Race Tensions", "Mixed",
  "Social Justice", "Black Empowerment", "Black Empowerment",
  "National Security", "Police Brutality", "Race Tensions",
  "Election", "Patriotism", "Black Empowerment",
  "Social Justice", "Minorities", "Race Tensions",
  "Police Brutality", "Patriotism", "Race Tensions",
  "Black Empowerment", "Black Empowerment", "Minorities",
  "Black Empowerment"
)

# Interests effects
prepInterests <- estimateEffect(
  formula = 1:49 ~ Interests,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)
makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_interests.pdf", 
  prep = prepInterests, 
  covar_name = "Interests", 
  right_val = "Right Wing",
  right_lab = "Right Wing",
  left_val = "Left Wing", 
  left_lab = "Left Wing")

# Ad spend bin effects
prepAdSpendBin <- estimateEffect(
  formula = 1:49 ~ AdSpendBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)
makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_ad_spend_bin.pdf", 
  prep = prepAdSpendBin, 
  covar_name = "AdSpendBin", 
  right_val = "high",
  right_lab = "High",
  left_val = "low", 
  left_lab = "Low")

# Age bin effects
prepAgeBin <- estimateEffect(
  formula = 1:49 ~ AgeAverageBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_age_average_bin.pdf", 
  prep = prepAgeBin, 
  covar_name = "AgeAverageBin", 
  right_val = "HighAge",
  right_lab = "High",
  left_val = "LowAge", 
  left_lab = "Low")

# Clicks bin effects
prepClicksBin <- estimateEffect(
  formula = 1:49 ~ ClicksBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_clicks_bin.pdf", 
  prep = prepClicksBin, 
  covar_name = "ClicksBin", 
  right_val = "high",
  right_lab = "High",
  left_val = "low", 
  left_lab = "Low")

# Impressions bin effects
prepImpressionsBin <- estimateEffect(
  formula = 1:49 ~ ImpressionsBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_impressions_bin.pdf", 
  prep = prepImpressionsBin, 
  covar_name = "ImpressionsBin", 
  right_val = "high",
  right_lab = "High",
  left_val = "low", 
  left_lab = "Low")

# Spend bin effects
prepAdSpendBin <- estimateEffect(
  formula = 1:49 ~ AdSpendBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_spend_bin.pdf", 
  prep = prepAdSpendBin, 
  covar_name = "AdSpendBin", 
  right_val = "high",
  right_lab = "High",
  left_val = "low", 
  left_lab = "Low")

# Account group cluster effects
accountGroupClusterBin <- estimateEffect(
  formula = 1:49 ~ AccountGroupCluster,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

makePrimaryTopicForestPlot(
  stm = stm, 
  file_name = "effects_account_group_cluster.pdf", 
  prep = accountGroupClusterBin, 
  covar_name = "AccountGroupCluster", 
  right_val = "cluster_2",
  right_lab = "Cluster Two",
  left_val = "cluster_1", 
  left_lab = "Cluster One")
