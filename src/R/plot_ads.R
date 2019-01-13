rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(tidyverse)
library(scales)
library(lubridate)
library(forestplot)
library(ggridges)
library(formatR)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(dendsort)
library(reshape2)
library(lattice)
library(latticeExtra)
library(ggrepel)
library(qgraph)
library(styler)
library(gplots)
library(ComplexHeatmap)
library(dendsort)

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

# PLOT _
# Plot grouped topics' clicks, impressions, spend
topic_metrics <- full %>%
  drop_na(primary_topic) %>%
  group_by(primary_topic) %>%
  summarise(
    sum_count = n(),
    sum_clicks = sum(Clicks),
    sum_impressions = sum(Impressions),
    sum_spend = sum(AdSpend)
  ) %>%
  mutate(
    pct_count = sum_count / sum(sum_count),
    pct_clicks = sum_clicks / sum(sum_clicks),
    pct_impressions = sum_impressions / sum(sum_impressions),
    pct_spend = sum_spend / sum(sum_spend)
  ) %>%
  select(primary_topic, pct_count, pct_clicks, pct_impressions, pct_spend)

pdf("~/Documents/thesis/data/figures/topic_metrics_spend_count.pdf")
topic_metrics %>%
  ggplot(aes(y = reorder(factor(primary_topic), pct_count))) +
  geom_point(aes(x = pct_spend, color = "spend")) +
  geom_point(aes(x = pct_count, color = "count")) +
  scale_colour_manual(
    name = "",
    values = c("count" = "#F44336", "spend" = "#EF9A9A"),
    labels = c("Count", "Spend")
  ) +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    axis.ticks = element_blank()
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

pdf("~/Documents/thesis/data/figures/topic_metrics_impressions_clicks.pdf")
topic_metrics %>%
  ggplot(aes(y = reorder(factor(primary_topic), pct_clicks))) +
  geom_point(aes(x = pct_impressions, color = "impressions")) +
  geom_point(aes(x = pct_clicks, color = "clicks")) +
  scale_colour_manual(
    name = "",
    values = c("clicks" = "#2196F3", "impressions" = "#90CAF9"),
    labels = c("Clicks", "Impressions")
  ) +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    axis.ticks = element_blank()
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

# PLOT _
# Plot grouped interests' circulation (count and spend) and reach (clicks and impressions)
interests_metrics <- full %>%
  drop_na(primary_topic) %>%
  filter(Interests != "unavailable") %>%
  group_by(Interests) %>%
  summarise(
    sum_count = n(),
    sum_clicks = sum(Clicks),
    sum_impressions = sum(Impressions),
    sum_spend = sum(AdSpend)
  ) %>%
  mutate(
    pct_count = sum_count / sum(sum_count),
    pct_clicks = sum_clicks / sum(sum_clicks),
    pct_impressions = sum_impressions / sum(sum_impressions),
    pct_spend = sum_spend / sum(sum_spend)
  ) %>%
  select(Interests, pct_count, pct_clicks, pct_impressions, pct_spend)

pdf("~/Documents/thesis/data/figures/interests_metrics_spend_count.pdf")
interests_metrics %>%
  ggplot(aes(y = reorder(factor(Interests), pct_count))) +
  geom_point(aes(x = pct_spend, color = "spend")) +
  geom_point(aes(x = pct_count, color = "count")) +
  scale_colour_manual(
    name = "",
    values = c("count" = "#F44336", "spend" = "#EF9A9A"),
    labels = c("Count", "Spend")
  ) +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    axis.ticks = element_blank()
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

pdf("~/Documents/thesis/data/figures/interests_metrics_impressions_clicks.pdf")
interests_metrics %>%
  ggplot(aes(y = reorder(factor(Interests), pct_clicks))) +
  geom_point(aes(x = pct_impressions, color = "impressions")) +
  geom_point(aes(x = pct_clicks, color = "clicks")) +
  scale_colour_manual(
    name = "",
    values = c("clicks" = "#2196F3", "impressions" = "#90CAF9"),
    labels = c("Clicks", "Impressions")
  ) +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    axis.ticks = element_blank()
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

# PLOT _
# Plot topics over time
event_date <- c(as.Date("2016-11-08"), as.Date("2016-03-16"), as.Date("2016-09-20"), as.Date("2016-06-06"))
event_pos <- c(15.65, 15.65, 15.65, 15.65)
event_label <- c("Election", "WikiLeaks Dump", "Keith Scott Killed", "Hillary Nominee")
event_annotes <- data.frame(event_date, event_pos, event_label)

pdf("~/Documents/thesis/data/figures/topics_date.pdf")
full %>%
  drop_na(primary_topic) %>%
  mutate() %>%
  ggplot(aes(x = as.Date(CreationDateFormatted, "%Y-%m-%d"), y = as.factor(primary_topic))) +
  geom_density_ridges2(aes(alpha = 0.5)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(title = "", x = "", y = "") +
  geom_segment(
    data = event_annotes, aes(
      x = event_date,
      xend = event_date,
      y = 0,
      yend = event_pos,
      color = "blue"
    ),
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = event_annotes,
    fill = "grey90",
    alpha = 0.90,
    aes(
      x = event_date,
      y = event_pos,
      label = event_label,
      hjust = -.1,
      lineheight = 0.9
    )
  )
dev.off()

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
makePrimaryTopicForestPlot <- function(plot, file_name, covar_label_1, covar_label_2) {
  
  plotDF <- data.frame(t(sapply(plot$cis, function(x) x[1:max(lengths(plot$cis))])))
  
  prepDF <- data.frame(
    plot$labels,
    plot$topics,
    unlist(plot$means),
    plotDF$X2.5.,
    plotDF$X97.5.)

  colnames(prepDF) <- c("primary_topic",
                        "topic_n",
                        "mean",
                        "lower",
                        "upper")

  prepDF <- prepDF %>%
    group_by(primary_topic) %>%
    summarise(
      avg_ci_point_estimate = mean(mean),
      avg_ci_lower_bound = mean(lower),
      avg_ci_upper_bound = mean(upper))

  table_text <- cbind(
    c(NA, seq(1, length(prepDF$primary_topic))),
    c("Primary Topic", as.vector(prepDF$primary_topic)),
    c("Confidence Interval (95%)", formatInterval(
      prepDF$avg_ci_point_estimate,
      prepDF$avg_ci_lower_bound,
      prepDF$avg_ci_upper_bound)))

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
             xlab = (str_c("\n", covar_label_1, " ... ", covar_label_2)),
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

# PLOT _
# Plot interest effects
prepInterests <- estimateEffect(
  formula = 1:49 ~ Interests,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)
pdf("~/Documents/thesis/data/figures/effects_interests.pdf")

plotInterests <- plot(prepInterests,
                      covariate = "Interests", 
                      topics = c(1:49),
                      model = stm, 
                      method = "difference",
                      cov.value1 = "Right Wing", 
                      cov.value2 = "Left Wing",
                      xlab = "Left wing ... Right wing",
                      main = "",
                      labeltype = "custom",
                      custom.labels = topicNames)
dev.off()

pdf("~/Documents/thesis/data/figures/effects_interests_3_8_20_37.pdf")
plot.STM(stm, topics = c(3, 8, 20, 37), type = "labels")
dev.off()

# PLOT _
# Plot interest effects (all)

topicNamesDf <- data.frame(
  topicnames = topicNames,
  TopicNumber = 1:49,
  TopicProportions = colMeans(stm$theta)
)

interestsAllUnordered <- plot(prepInterests,
  covariate = "Interests",
  topics = c(1:49),
  model = stm,
  method = "difference",
  cov.value1 = "Right Wing",
  cov.value2 = "Left Wing",
  xlab = "Left wing ... Right wing",
  main = ""
)
rank <- order(unlist(interestsAllUnordered$means))
topicNamesDf <- topicNamesDf[rank, ]
par(mfrow = c(1, 1), mar = c(6, 6, 4, 4))

pdf("~/Documents/thesis/data/figures/effects_interests_all.pdf", height = 10.5)
plot(prepInterests, "Interests",
  method = "difference",
  cov.value1 = "Right Wing",
  cov.value2 = "Left Wing",
  topics = topicNamesDf$TopicNumber,
  verbose.labels = F,
  labeltype = "custom",
  xlab = "Left wing ... Right wing",
  custom.labels = topicNamesDf$topicnames,
  main = "",
  xlim = c(-.25, .15)
)
dev.off()

# PLOT _
# Plot ad spend bin effects
prepAdSpendBin <- estimateEffect(
  formula = 1:49 ~ AdSpendBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)
pdf("~/Documents/thesis/data/figures/effects_bin_adspend.pdf")
plot(prepAdSpendBin,
  covariate = "AdSpendBin",
  topics = c(2, 21, 37),
  model = stm,
  method = "difference",
  cov.value1 = "high",
  cov.value2 = "low",
  xlab = "Lower ... Higher",
  main = "",
  labeltype = "custom",
  xlim = c(-.1, .1),
  custom.labels = c("Black Empowerment", "Bearing Arms", "Election")
)
dev.off()

pdf("~/Documents/thesis/data/figures/effects_bin_adspend_2_21_37.pdf")
plot.STM(stm, topics = c(2, 21, 37), type = "labels", width = 50)
dev.off()

# PLOT _
# Plot ad spend bin effects (all)

topicNamesDf <- data.frame(
  topicnames = topicNames,
  TopicNumber = 1:49,
  TopicProportions = colMeans(stm$theta)
)

adSpendBinAllUnordered <- plot(prepAdSpendBin,
  covariate = "AdSpendBin",
  topics = c(1:49),
  model = stm,
  method = "difference",
  cov.value1 = "high",
  cov.value2 = "low",
  xlab = "Lower ... Higher",
  main = "",
  xlim = c(-.1, .1)
)
rank <- order(unlist(adSpendBinAllUnordered$means))
topicNamesDf <- topicNamesDf[rank, ]
par(mfrow = c(1, 1), mar = c(6, 6, 4, 4))

pdf("~/Documents/thesis/data/figures/effects_bin_adspend_all.pdf", height = 10.5)
plot(prepAdSpendBin, "AdSpendBin",
  method = "difference",
  cov.value1 = "high",
  cov.value2 = "low",
  topics = topicNamesDf$TopicNumber,
  verbose.labels = F,
  labeltype = "custom",
  xlab = "Lower ... Higher",
  custom.labels = topicNamesDf$topicnames,
  main = "",
  xlim = c(-.1, .1)
)
dev.off()

# PLOT _
# Plot age bin effects
prepAgeBin <- estimateEffect(
  formula = 1:49 ~ AgeAverageBin,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

pdf("~/Documents/thesis/data/figures/effects_bin_age.pdf")
plot(prepAgeBin,
  covariate = "AgeAverageBin",
  topics = c(8, 44, 47),
  model = stm,
  method = "difference",
  cov.value1 = "HighAge",
  cov.value2 = "LowAge",
  xlab = "Lower ... Higher",
  main = "",
  labeltype = "custom",
  xlim = c(-.2, .4),
  custom.labels = c("Race Tensions", "Patriotism", "Black Empowerment")
)
dev.off()

pdf("~/Documents/thesis/data/figures/effects_bin_age_8_44_47.pdf")
plot.STM(stm, topics = c(8, 44, 47), type = "labels", width = 50)
dev.off()

# PLOT _
# Plot age effects (all)
topicNamesDf <- data.frame(
  topicnames = topicNames,
  TopicNumber = 1:49,
  TopicProportions = colMeans(stm$theta)
)

ageAllUnordered <- plot(prepAgeBin,
  covariate = "AgeAverageBin",
  topics = c(1:49),
  model = stm,
  method = "difference",
  cov.value1 = "HighAge",
  cov.value2 = "LowAge",
  xlab = "Lower ... Higher",
  main = "",
  xlim = c(-.1, .1)
)
rank <- order(unlist(ageAllUnordered$means))
topicNamesDf <- topicNamesDf[rank, ]
par(mfrow = c(1, 1), mar = c(6, 6, 4, 4))

pdf("~/Documents/thesis/data/figures/effects_bin_age_all.pdf", height = 10.5)
plot(prepAgeBin, "AgeAverageBin",
  method = "difference",
  cov.value1 = "HighAge",
  cov.value2 = "LowAge",
  topics = topicNamesDf$TopicNumber,
  verbose.labels = F,
  labeltype = "custom",
  xlab = "Lower ... Higher",
  custom.labels = topicNamesDf$topicnames,
  main = "",
  xlim = c(-.3, .4)
)
dev.off()

# PLOT _
# Plot age quant effects
prepAgeQuant <- estimateEffect(
  formula = 1:49 ~ AgeAverage,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

pdf("~/Documents/thesis/data/figures/effects_quant_age.pdf")
plot(prepAgeQuant, "AgeAverage",
  method = "continuous",
  topics = c(17, 8),
  model = stm,
  printlegend = F,
  linecol = c("blue", "red"),
  xlab = "Targeted Age (Average)",
  labeltype = "custom",
  custom.labels = c("Police Brutality", "Race Tensions")
)

legend(
  x = 41,
  y = -0.04,
  c("Police Brutality", "Race Tensions"),
  lwd = 2,
  cex = 1,
  col = c("blue", "red")
)
dev.off()


# PLOT _
# Plot topics relative to interests
# (Heatmap)
interestTopicDf <- full %>%
  select(c("Interests", "primary_topic")) %>%
  filter(Interests != "mixed" & Interests != "unavailable" & primary_topic != "Mixed")

interestTopicMatrix <- t(as.matrix(table(droplevels(interestTopicDf))))

d <- ifelse(log(interestTopicMatrix) < 0, 0, log(interestTopicMatrix))

dd.row <- as.dendrogram(hclust(dist(d)))
row.ord <- order.dendrogram(dd.row)
dd.col <- as.dendrogram(hclust(dist(t(d))))
col.ord <- order.dendrogram(dd.col)

lattice.options(axis.padding = list(factor = 0.5))

# pdf("~/Documents/thesis/data/figures/interests_topics_heatmap.pdf")
levelplot(d[row.ord, col.ord],
  aspect = "fill",
  xlab = "Discussed Topics",
  ylab = "Targeted Interests",
  pretty = TRUE,
  drop.unused.levels = TRUE,
  scales = list(x = list(rot = 45), tck = c(0, 0)),
  colorkey = list(space = "right"),
  par.settings = custom.theme(region = plasma(10)),
  border = "black",
  border.lwd = .6,
  xaxt = "n",
  yaxt = "n",
  legend = list(
    left = list(
      fun = dendrogramGrob, args =
        list(
          x = dd.col, ord = col.ord,
          side = "right",
          size = 0
        )
    ),
    top = list(
      fun = dendrogramGrob, args =
        list(
          x = dd.row,
          side = "top", size = 0
        )
    )
  )
)

# dev.off()

# PLOT _
# Plot group network
# THESE WERE COMPUTED WITH JUPYTER NOTEBOOK
edges <- read.csv("csv/group_to_group.csv")
nodes <- read.csv("csv/group_node.csv")

edges_spread <- edges %>%
  spread(group2, weight)
edges_spread[is.na(edges_spread)] <- 0

cormatrix <- cor_auto(edges_spread)

group_sizes <- full %>%
  select("AccountGroup") %>%
  group_by(AccountGroup) %>%
  summarise(rank = n()) %>%
  ungroup() %>%
  filter(AccountGroup %in% colnames(cormatrix)) %>%
  rowwise() %>%
  mutate(rank = max((log2(rank) / 2) + 1, 2))

nums <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
  "31", "32", "33", "34", "35", "36", "37", "38", "39", "40",
  "41", "42", "43", "44", "45", "46", "47", "48", "49", "50",
  "51", "52", "53", "54", "55", "56", "57", "58", "59", "60"
)

qgraph(cormatrix,
  layout = "spring",
  theme = "Borkulo",
  nodeNames = colnames(cormatrix),
  labels = nums,
  vsize = group_sizes$rank,
  filetype = "pdf",
  filename = "~/Documents/coolplot",
  cut = .9,
  minimum = .5,
  width = 17.5,
  height = 17.5,
  legend = TRUE,
  legend.mode = "names",
  normalize = TRUE,
  # XKCD = TRUE, # the most important argument
  theme = "Borkulo"
)

# Get 7 groups from correlation matrix
# ega <- EGA(cormatrix, n=61, plot.EGA=TRUE)

# Matrix heatmap
cormatrix_original_labels <- colnames(cormatrix)
cormatrix_new_labels <- sprintf("%s", seq(1:60))
colnames(cormatrix) <- cormatrix_new_labels
rownames(cormatrix) <- cormatrix_new_labels

coolmap <- Heatmap(
  cormatrix,
  clustering_distance_rows = function(x) as.dist(1 - cor(t(x))),
  clustering_distance_columns = function(x) as.dist(1 - cor(t(x))),
  clustering_method_columns = "average",
  clustering_method_rows = "average",
  row_dend_reorder = TRUE,
  column_dend_reorder = TRUE,
  row_dend_width = unit(2, "in"),
  column_dend_height = unit(2, "in"),
  # km = 2,
  name = " ",
  heatmap_legend_param = list(
    legend_width = unit(5, "in"),
    legend_direction = "horizontal"
  )
)

pdf("~/Documents/coolmap.pdf", height = 14, width = 14)
draw(coolmap, heatmap_legend_side = "bottom")
dev.off()

# Cluster topic prevalence analysis
clusterPrep <- estimateEffect(
  formula = c(1:49) ~ AccountGroupCluster,
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)

clusterPrepPlot <- plot(clusterPrep,
  covariate = "AccountGroupCluster",
  topics = c(1:49),
  model = stm,
  method = "difference",
  cov.value1 = "cluster_2",
  cov.value2 = "cluster_1",
  xlab = "Cluster one ... Cluster two",
  main = "",
  labeltype = "custom",
  xlim = c(-.3, .3),
  custom.labels = topicNames
)

clusterPrepPlotDf <- data.frame(t(sapply(clusterPrepPlot$cis, function(x) x[1:max(lengths(clusterPrepPlot$cis))])))
clusterPrepDf <- data.frame(
  clusterPrepPlot$labels,
  clusterPrepPlot$topics,
  unlist(clusterPrepPlot$means),
  clusterPrepPlotDf$X2.5.,
  clusterPrepPlotDf$X97.5.
)
colnames(clusterPrepDf) <- c("primary_topic", "topic_n", "mean", "lower", "upper")
clusterPrepDf <- clusterPrepDf %>%
  group_by(primary_topic) %>%
  summarise(
    avg_ci_point_estimate = mean(mean),
    avg_ci_lower_bound = mean(lower),
    avg_ci_upper_bound = mean(upper)
  )

table_text <- cbind(
  c(NA, seq(1, length(clusterPrepDf$primary_topic))),
  c("Primary Topic", as.vector(clusterPrepDf$primary_topic)),
  c("Confidence Interval (95%)", formatInterval(
    clusterPrepDf$avg_ci_point_estimate,
    clusterPrepDf$avg_ci_lower_bound,
    clusterPrepDf$avg_ci_upper_bound)))

pdf("~/Documents/coolforest.pdf")
forestplot(table_text,
  graph.pos = 3,
  is.summary = c(TRUE, rep(FALSE, length(clusterPrepDf$primary_topic))),
  align = c("r", "l", "r"),
  mean = c(NA, clusterPrepDf$avg_ci_point_estimate),
  lower = c(NA, clusterPrepDf$avg_ci_lower_bound),
  upper = c(NA, clusterPrepDf$avg_ci_upper_bound),
  fn.ci_norm = fpDrawCircleCI,
  hrzl_lines = list("2" = gpar(lwd = 2, col = "#000000")),
  boxsize = .15,
  xlab = ("\nCluster One ... Cluster Two"),
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

# PLOT _
# Basic summary graphs
metric_df <- full %>%
  select(Impressions, Clicks, AdSpend) %>%
  filter(Impressions < quantile(Impressions, 0.98) & Impressions > 0) %>%
  filter(Clicks < quantile(Clicks, 0.98) & Clicks > 0) %>%
  filter(AdSpend < quantile(AdSpend, 0.98) & AdSpend > 0)

pdf("~/Documents/thesis/data/figures/impressions.pdf")
impressions_p <- metric_df %>%
  ggplot() +
  geom_histogram(aes(Impressions, y = ..count.. / sum(..count..)),
    binwidth = quantile(full$Impressions, 0.95) * 0.05,
    colour = "white",
    size = 1,
    boundary = 0
  ) +
  xlim(0, quantile(full$Impressions, 0.95)) +
  ylim(0, .55) +
  ylab("") +
  xlab("Impressions")

impressions_p_sub <- full %>%
  filter(Impressions < quantile(Impressions, 0.99)) %>%
  ggplot() +
  geom_density(aes(Impressions, ..count.. / sum(..count..))) +
  ylab("") +
  xlab("")

ggdraw() +
  draw_plot(impressions_p + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(impressions_p_sub +
    theme(legend.justification = "top"), 0.5, 0.5, 0.5, 0.5)
# full %>%
#   ggplot() +
#   geom_histogram(aes(Impressions,y = ..count../sum(..count..)),
#                  binwidth = 1000,
#                  colour='white',
#                  size=1) +
#   xlim(0,20000) +
#   ylim(0,.15) +
#   ylab('Proportion') +
#   xlab('Ad Impressions')
dev.off()

pdf("~/Documents/thesis/data/figures/clicks.pdf")
clicks_p <- metric_df %>%
  ggplot() +
  geom_histogram(aes(Clicks, y = ..count.. / sum(..count..)),
    binwidth = quantile(full$Clicks, 0.95) * 0.05,
    colour = "white",
    size = 1,
    boundary = 0
  ) +
  xlim(0, quantile(full$Clicks, 0.95)) +
  ylim(0, .55) +
  ylab("") +
  xlab("Clicks")

clicks_p_sub <- full %>%
  filter(Clicks < quantile(Clicks, 0.99)) %>%
  ggplot() +
  geom_density(aes(Clicks, ..count.. / sum(..count..))) +
  ylab("") +
  xlab("")

ggdraw() +
  draw_plot(clicks_p + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(clicks_p_sub +
    theme(legend.justification = "top"), 0.5, 0.5, 0.5, 0.5)
# full %>%
#   ggplot() +
#   geom_histogram(aes(Clicks,y = ..count../sum(..count..)),
#                  binwidth = 1000,
#                  colour='white',
#                  size=1) +
#   xlim(0,13000) +
#   ylim(0,.15) +
#   ylab('Proportion') +
#   xlab('Ad Clicks')
dev.off()

pdf("~/Documents/thesis/data/figures/spend.pdf")
spend_p <- metric_df %>%
  ggplot() +
  geom_histogram(aes(AdSpend, y = ..count.. / sum(..count..)),
    binwidth = quantile(full$AdSpend, 0.95) * 0.05,
    colour = "white",
    size = 1,
    boundary = 0
  ) +
  xlim(0, quantile(full$AdSpend, 0.95)) +
  ylim(0, 0.55) +
  ylab("") +
  xlab("Spend (RUB)")

spend_p_sub <- full %>%
  filter(AdSpend < quantile(AdSpend, 0.99)) %>%
  ggplot() +
  geom_density(aes(AdSpend, ..count.. / sum(..count..))) +
  ylab("") +
  xlab("")

ggdraw() +
  draw_plot(spend_p + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(spend_p_sub +
    theme(legend.justification = "top"), 0.5, 0.5, 0.5, 0.5)
# full %>%
#   ggplot() +
#   geom_histogram(aes(AdSpend,y = ..count../sum(..count..)),
#                  binwidth = 1000,
#                  colour='white',
#                  size=1) +
#   xlim(0,25000) +
#   ylim(0,.15) +
#   ylab('Proportion') +
#   xlab('Ad Spend (RUB)')
dev.off()

# PLOT _
# Plot groups relative to topics
# (Heatmap)
accountGroupTopicDf <- full %>%
  select(c("AccountGroup", "primary_topic")) %>%
  filter(AccountGroup != "unavailable" & primary_topic != "Mixed")

accountGroupTopicMatrix <- t(as.matrix(table(droplevels(accountGroupTopicDf))))

d2 <- ifelse(log10(accountGroupTopicMatrix) < 0, 0, log10(accountGroupTopicMatrix))

dd2.row <- as.dendrogram(hclust(dist(d2)))
row2.ord <- order.dendrogram(dd2.row)
dd2.col <- as.dendrogram(hclust(dist(t(d2))))
col2.ord <- order.dendrogram(dd2.col)

lattice.options(axis.padding = list(factor = 0.5))

levelplot(d2[row2.ord, col2.ord],
  aspect = "fill",
  xlab = "Discussed Topics",
  ylab = "Account Groups",
  pretty = TRUE,
  drop.unused.levels = TRUE,
  scales = list(x = list(rot = 45), tck = c(0, 0)),
  colorkey = list(space = "right"),
  par.settings = custom.theme(region = plasma(10)),
  border = "black",
  border.lwd = .6,
  xaxt = "n",
  yaxt = "n",
  legend = list(
    left = list(
      fun = dendrogramGrob, args =
        list(
          x = dd2.col, ord = col2.ord,
          side = "right",
          size = 0
        )
    ),
    top = list(
      fun = dendrogramGrob, args =
        list(
          x = dd2.row,
          side = "top", size = 0
        )
    )
  )
)

# PLOT _
# Effects of time on topics
creationDatePrep <- estimateEffect(
  formula = c(1:49) ~ s(CreationDateInteger),
  stmobj = stm,
  metadata = out$meta,
  uncertainty = "Global"
)
pdf("~/Documents/thesis/data/figures/date_topic27.pdf")
plot(creationDatePrep, "CreationDateInteger",
  method = "continuous", topics = (27),
  model = stm, printlegend = TRUE,
  xlab = "Time",
  xaxt = "n",
  labeltype = "custom",
  custom.labels = c("Black Empowerment (Topic 27)")
)

monthseq <- seq(
  from = as.Date("2014-01-01"),
  to = as.Date("2019-08-13"), by = "month"
)
monthnames <- months(monthseq)
axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)), labels = monthnames)
dev.off()

# PLOT _
# Time series trends
time_df <- full
time_df$Date <- as.Date(
  time_df$CreationDateFormatted,
  "%Y-%m-%d"
)
time_df$Month <- as.Date(cut(time_df$Date,
  breaks = "week"
))

time_df <- time_df %>%
  select(primary_topic, Month) %>%
  group_by(Month, primary_topic) %>%
  summarise(n = n()) %>%
  group_by(primary_topic) %>%
  mutate(freq = n / sum(n)) %>%
  filter(!is.na(primary_topic))

time_df %>%
  filter(primary_topic == "Election") %>%
  ggplot(aes(Month, n)) +
  geom_point() +
  geom_path() +
  geom_vline(aes(xintercept = as.Date("2016-11-08")), colour = "blue")


# PLOT _

# PLOT _ Topic quality
# topicQuality(stm, docs, xlab = "Semantic Coherence", ylab = "Exclusivity", labels = 1:ncol(stm$theta), M = 10)
