rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(tidyverse)
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
# library(Matrix)
# library(igraph)
# library(GGally)
# library(network)
# library(sna)
library(qgraph)
library(tidyr)


# Read data
fb <- read.csv('csv/fb_gold.csv')

# Process data
processed <- textProcessor(documents = fb$AdText, metadata = fb)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# Load full, stm
full <- read.csv('csv/df_interests_spline_age_49.csv')
stm <- readRDS("~/Documents/thesis/data/rds/model_interests_spline_age_49.RDS")

# Set topic names
topicNames <- c("Race Tensions","Black Empowerment","Black Empowerment","Black Empowerment","Incarceration","Communal Support","Race Tensions","Race Tensions","Mixed","Islam in America","Race Tensions","Black Empowerment","Black Empowerment","Mixed","Mixed","Police Brutality","Police Brutality","Race Tensions","Police Brutality","Patriotism","Bearing Arms","Social Justice","Mixed","Mixed","Music Streaming","Patriotism","Black Empowerment","Black Empowerment","Race Tensions","Mixed","Social Justice","Black Empowerment","Black Empowerment","National Security","Police Brutality","Race Tensions","Election","Patriotism","Black Empowerment","Social Justice","Minorities","Race Tensions","Police Brutality","Patriotism","Race Tensions","Black Empowerment","Black Empowerment","Minorities","Black Empowerment")

# PLOT _
# Plot grouped topics' clicks, impressions, spend
topic_metrics <- full %>% 
  drop_na(primary_topic) %>%
  group_by(primary_topic) %>%
  summarise(sum_count = n(),
            sum_clicks= sum(Clicks),
            sum_impressions = sum(Impressions),
            sum_spend = sum(AdSpend)) %>% 
  mutate(pct_count = sum_count/sum(sum_count),
         pct_clicks = sum_clicks/sum(sum_clicks),
         pct_impressions = sum_impressions/sum(sum_impressions),
         pct_spend = sum_spend/sum(sum_spend)) %>% 
  select(primary_topic,pct_count,pct_clicks,pct_impressions,pct_spend)

pdf('~/Documents/thesis/data/figures/topic_metrics_spend_count.pdf')
topic_metrics %>% 
  ggplot(aes(y = reorder(factor(primary_topic), pct_count))) +
  geom_point(aes(x = pct_spend, color = "spend")) +
  geom_point(aes(x = pct_count, color="count")) +
  scale_colour_manual(
    name = '', 
    values = c('count'='#F44336', 'spend' = '#EF9A9A'), 
    labels = c('Count', 'Spend')) +
  labs(title = "", x = "", y="") +
  theme(legend.position='top', 
        legend.justification='center',
        legend.direction='horizontal',
        axis.ticks = element_blank()) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

pdf('~/Documents/thesis/data/figures/topic_metrics_impressions_clicks.pdf')
topic_metrics %>% 
  ggplot(aes(y = reorder(factor(primary_topic), pct_clicks))) +
  geom_point(aes(x = pct_impressions, color = "impressions")) +
  geom_point(aes(x = pct_clicks, color="clicks")) +
  scale_colour_manual(
    name = '', 
    values = c('clicks'='#2196F3', 'impressions' = '#90CAF9'), 
    labels = c('Clicks', 'Impressions')) +
  labs(title = "", x = "", y="") +
  theme(legend.position='top', 
        legend.justification='center',
        legend.direction='horizontal',
        axis.ticks = element_blank()) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

# PLOT _
# Plot grouped interests' circulation (count and spend) and reach (clicks and impressions)
interests_metrics <- full %>% 
  drop_na(primary_topic) %>%
  filter(Interests != "unavailable") %>% 
  group_by(Interests) %>%
  summarise(sum_count = n(),
            sum_clicks= sum(Clicks),
            sum_impressions = sum(Impressions),
            sum_spend = sum(AdSpend)) %>% 
  mutate(pct_count = sum_count/sum(sum_count),
         pct_clicks = sum_clicks/sum(sum_clicks),
         pct_impressions = sum_impressions/sum(sum_impressions),
         pct_spend = sum_spend/sum(sum_spend)) %>% 
  select(Interests,pct_count,pct_clicks,pct_impressions,pct_spend)

pdf('~/Documents/thesis/data/figures/interests_metrics_spend_count.pdf')
interests_metrics %>% 
  ggplot(aes(y = reorder(factor(Interests), pct_count))) +
  geom_point(aes(x = pct_spend, color = "spend")) +
  geom_point(aes(x = pct_count, color="count")) +
  scale_colour_manual(
    name = '', 
    values = c('count'='#F44336', 'spend' = '#EF9A9A'), 
    labels = c('Count', 'Spend')) +
  labs(title = "", x = "", y="") +
  theme(legend.position='top', 
        legend.justification='center',
        legend.direction='horizontal',
        axis.ticks = element_blank()) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

pdf('~/Documents/thesis/data/figures/interests_metrics_impressions_clicks.pdf')
interests_metrics %>% 
  ggplot(aes(y = reorder(factor(Interests), pct_clicks))) +
  geom_point(aes(x = pct_impressions, color = "impressions")) +
  geom_point(aes(x = pct_clicks, color="clicks")) +
  scale_colour_manual(
    name = '', 
    values = c('clicks'='#2196F3', 'impressions' = '#90CAF9'), 
    labels = c('Clicks', 'Impressions')) +
  labs(title = "", x = "", y="") +
  theme(legend.position='top', 
        legend.justification='center',
        legend.direction='horizontal',
        axis.ticks = element_blank()) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
dev.off()

# PLOT _
# Plot topics over time
event_date <- c(as.Date("2016-11-08"), as.Date('2016-03-16'), as.Date('2016-09-20'))
event_pos <- c(15.65,15.65,15.65)
event_label <- c('Election','WikiLeaks Dump','Keith Scott Killed')
event_annotes <- data.frame(event_date, event_pos, event_label)

pdf('~/Documents/thesis/data/figures/topics_date.pdf')
full %>% 
  drop_na(primary_topic) %>%
  ggplot(aes(x = as.Date(CreationDateFormatted, "%Y-%m-%d"), y = as.factor(primary_topic))) + 
  geom_density_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +  
  labs(title = "", x = "", y = "") +
  geom_segment(data = event_annotes, aes(
    x = event_date, 
    xend = event_date,
    y = 0,
    yend = event_pos, 
    color = "blue"),
    show.legend = FALSE) +
  geom_label_repel(data = event_annotes,
                   fill = "grey90",
                   alpha = 0.90,
                   aes(x = event_date, 
                       y = event_pos, 
                       label = event_label,
                       hjust = -.1,
                       lineheight = 0.9))
dev.off()

# PLOT _
# Plot interest effects
prepInterests <- estimateEffect(formula = 1:49 ~ Interests, 
                                stmobj = stm,
                                metadata = out$meta, 
                                uncertainty = "Global")
pdf('~/Documents/thesis/data/figures/effects_interests.pdf')
plot(prepInterests, covariate = "Interests", topics = c(3, 8, 20, 37),
     model = stm, method = "difference",
     cov.value1 = "Right Wing", cov.value2 = "Left Wing",
     xlab = "Left wing ... Right wing",
     main = "",
     labeltype = "custom",
     xlim = c(-.15, .13),
     custom.labels = c('Black Empowerment', 'Race Tensions', 'Patriotism', 'Election'))
dev.off()

pdf('~/Documents/thesis/data/figures/effects_interests_3_8_20_37.pdf')
plot.STM(stm, topics = c(3,8,20,37), type = 'labels')
dev.off()

# PLOT _
# Plot interest effects (all)

topicNamesDf <- data.frame(topicnames = topicNames, 
                           TopicNumber = 1:49, 
                           TopicProportions = colMeans(stm$theta))

interestsAllUnordered <- plot(prepInterests, 
                              covariate = "Interests", 
                              topics = c(1:49),
                              model = stm,
                              method = "difference",
                              cov.value1 = "Right Wing", 
                              cov.value2 = "Left Wing",
                              xlab = "Left wing ... Right wing",
                              main = "")
rank = order(unlist(interestsAllUnordered$means))
topicNamesDf <- topicNamesDf[rank,]
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))

pdf('~/Documents/thesis/data/figures/effects_interests_all.pdf',height=10.5)
plot(prepInterests, "Interests", method = "difference", 
     cov.value1 = "Right Wing", 
     cov.value2 = "Left Wing", 
     topics = topicNamesDf$TopicNumber,
     verbose.labels = F, 
     labeltype = "custom",
     xlab = "Left wing ... Right wing",
     custom.labels  = topicNamesDf$topicnames, 
     main = "",
     xlim = c(-.25,.15))
dev.off()

# PLOT _
# Plot ad spend bin effects
prepAdSpendBin <- estimateEffect(formula = 1:49 ~ AdSpendBin,
                                 stmobj = stm,
                                 metadata = out$meta, 
                                 uncertainty = "Global")
pdf('~/Documents/thesis/data/figures/effects_bin_adspend.pdf')
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
     custom.labels = c('Black Empowerment', 'Bearing Arms', 'Election'))
dev.off()

pdf('~/Documents/thesis/data/figures/effects_bin_adspend_2_21_37.pdf')
plot.STM(stm, topics = c(2, 21, 37), type = 'labels',width=50)
dev.off()

# PLOT _
# Plot ad spend bin effects (all)

topicNamesDf <- data.frame(topicnames = topicNames, 
                           TopicNumber = 1:49, 
                           TopicProportions = colMeans(stm$theta))

adSpendBinAllUnordered <-plot(prepAdSpendBin,
                              covariate = "AdSpendBin", 
                              topics = c(1:49),
                              model = stm, 
                              method = "difference",
                              cov.value1 = "high", 
                              cov.value2 = "low",
                              xlab = "Lower ... Higher",
                              main = "",
                              xlim = c(-.1, .1))
rank = order(unlist(adSpendBinAllUnordered$means))
topicNamesDf <- topicNamesDf[rank,]
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))

pdf('~/Documents/thesis/data/figures/effects_bin_adspend_all.pdf',height=10.5)
plot(prepAdSpendBin, "AdSpendBin", method = "difference", 
     cov.value1 = "high", 
     cov.value2 = "low", 
     topics = topicNamesDf$TopicNumber,
     verbose.labels = F, 
     labeltype = "custom",
     xlab = "Lower ... Higher",
     custom.labels  = topicNamesDf$topicnames, 
     main = "",
     xlim = c(-.1,.1))
dev.off()

# PLOT _
# Plot ad spend quant effects

# prepAdSpendQuant <- estimateEffect(formula = 1:49 ~ s(AdSpend),
#                                    stmobj = stm,
#                                    metadata = out$meta, 
#                                    uncertainty = "Global")

# plot(prepAdSpendQuant, "AdSpend", 
#      method = "continuous", 
#      topics = c(21,37),
#      model = stm, 
#      printlegend = F, 
#      linecol = c("blue", "red"),
#      xlab = "Ad Spend (RUB)",
#      labeltype = "custom")
# 
# legend(x = 15, 
#        y = 0.6, 
#        c("Bearing Arms", 'Election'), 
#        lwd = 2,
#        cex = 0.5,
#        col = c("blue", "red"))

# PLOT _
# Plot age bin effects
prepAgeBin <- estimateEffect(formula = 1:49 ~ AgeAverageBin,
                             stmobj = stm,
                             metadata = out$meta, 
                             uncertainty = "Global")

pdf('~/Documents/thesis/data/figures/effects_bin_age.pdf')
plot(prepAgeBin, 
     covariate = "AgeAverageBin", 
     topics = c(8,44,47),
     model = stm, 
     method = "difference",
     cov.value1 = "HighAge", 
     cov.value2 = "LowAge",
     xlab = "Lower ... Higher",
     main = "",
     labeltype = "custom",
     xlim = c(-.2, .4),
     custom.labels = c('Race Tensions', 'Patriotism', 'Black Empowerment'))
dev.off()

pdf('~/Documents/thesis/data/figures/effects_bin_age_8_44_47.pdf')
plot.STM(stm, topics = c(8,44,47), type = 'labels',width=50)
dev.off()

# PLOT _
# Plot age effects (all)

topicNamesDf <- data.frame(topicnames = topicNames, 
                           TopicNumber = 1:49, 
                           TopicProportions = colMeans(stm$theta))

ageAllUnordered <-plot(prepAgeBin,
                       covariate = "AgeAverageBin",
                       topics = c(1:49),
                       model = stm, 
                       method = "difference",
                       cov.value1 = "HighAge", 
                       cov.value2 = "LowAge",
                       xlab = "Lower ... Higher",
                       main = "",
                       xlim = c(-.1, .1))
rank = order(unlist(ageAllUnordered$means))
topicNamesDf <- topicNamesDf[rank,]
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))

pdf('~/Documents/thesis/data/figures/effects_bin_age_all.pdf',height=10.5)
plot(prepAgeBin, "AgeAverageBin", method = "difference", 
     cov.value1 = "HighAge", 
     cov.value2 = "LowAge", 
     topics = topicNamesDf$TopicNumber,
     verbose.labels = F, 
     labeltype = "custom",
     xlab = "Lower ... Higher",
     custom.labels  = topicNamesDf$topicnames, 
     main = "",
     xlim = c(-.3,.4))
dev.off()

# PLOT _
# Plot age quant effects
prepAgeQuant <- estimateEffect(formula = 1:49 ~ AgeAverage,
                               stmobj = stm,
                               metadata = out$meta,
                               uncertainty = "Global")

pdf('~/Documents/thesis/data/figures/effects_quant_age.pdf')
plot(prepAgeQuant, "AgeAverage", 
     method = "continuous", 
     topics = c(8,37),
     model = stm, 
     printlegend = F, 
     linecol = c("blue", "red"),
     xlab = "Targeted Age (Average)",
     labeltype = "custom",
     custom.labels = c('Race Tensions', 'Music Streaming'))

legend(x = 15, 
       y = 0.08, 
       c("Race Tensions", "Music Streaming"), 
       lwd = 2,
       cex = 1,
       col = c("blue", "red"))
dev.off()


# PLOT _
# Plot topics relative to interests
# (Heatmap)

interestTopicDf <- full %>% 
  select(c('Interests', 'primary_topic')) %>% 
  filter(Interests != "mixed" & Interests != 'unavailable' & primary_topic != 'Mixed')

interestTopicMatrix <- t(as.matrix(table(droplevels(interestTopicDf))))

d <- ifelse(log2(interestTopicMatrix)<0,0,log10(interestTopicMatrix))

dd.row <- as.dendrogram(hclust(dist(d)))
row.ord <- order.dendrogram(dd.row)
dd.col <- as.dendrogram(hclust(dist(t(d))))
col.ord <- order.dendrogram(dd.col)

lattice.options(axis.padding=list(factor=0.5))

pdf('~/Documents/thesis/data/figures/interests_topics_heatmap.pdf')
levelplot(d[row.ord, col.ord],
          aspect = "fill", 
          xlab='Discussed Topics',
          ylab='Targeted Interests',
          pretty = TRUE,
          drop.unused.levels = TRUE,
          scales = list(x = list(rot = 45), tck=c(0,0)),
          colorkey = list(space = "right"),
          par.settings=custom.theme(region=plasma(10)),
          border='black', 
          border.lwd=.6,
          xaxt="n",
          yaxt="n",
          legend = list(left = list(fun = dendrogramGrob, args =
                          list(x = dd.col, ord = col.ord,
                               side = "right",
                               size = 0)),
                 top = list(fun = dendrogramGrob, args =
                          list(x = dd.row,
                               side = "top", size=0))))

dev.off()

# PLOT _ 
# Plot group network
# THESE WERE COMPUTED WITH JUPYTER NOTEBOOK
edges <- read.csv("csv/group_to_group.csv")
nodes <- read.csv("csv/group_node.csv")

nums <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52')


edges_spread <- edges %>% 
  spread(group1, weight)
edges_spread[is.na(edges_spread)] <- 0

cormatrix <- cor_auto(edges_spread)

graph1 <- qgraph(cormatrix, graph="glasso", layout="spring", sampleSize = nrow(edges_spread),
               vsize=7, cut=0, maximum=.45, border.width=1.5,
               labels = nums,
               vsize = 2,
               node.width = .51,
               node.heigt = .51,
               filetype='pdf',
               filename="~/Documents/coolplot3",
               width=15,
               height=15,
               legend=TRUE,
               legend.mode="names",
               nodeNames = colnames(cormatrix))

# PLOT _
# Different words associated with topic 
# stmContent <- stm(out$documents, out$vocab, K = 49,
#                   content =~ Interests,
#                   max.em.its = 75, data = out$meta, init.type = "Spectral")
# plot(stm, type = "perspectives", topics = 10)


# PLOT _ Topic quality
# topicQuality(stm, docs, xlab = "Semantic Coherence", ylab = "Exclusivity", labels = 1:ncol(stm$theta), M = 10)


# PLOT _
# Plot topics by count
# full %>% 
#   drop_na(primary_topic) %>%
#   ggplot(aes(x = factor(primary_topic, levels=names(sort(table(primary_topic)))))) +
#   geom_bar() +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,1)) +
#   labs(title = "Grouped Topic Counts", x = "", y = "")
