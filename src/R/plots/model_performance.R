rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(cowplot)

searchk_results <- readRDS('rds/searchkresults.RDS')
searchk_df <- searchk_results$results

# Exlcusivity
pdf('~/Documents/thesis/data/figures/searchk_exclusivity.pdf')
searchk_df %>% 
  ggplot() +
  geom_line(aes(x = K, y = exclus)) +
  geom_point(aes(x = K, y = exclus)) +
  labs(x = '\nTopics', y = 'Exclusivity\n') +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 23)) 
dev.off()


# Heldout likelihood
pdf('~/Documents/thesis/data/figures/searchk_heldout.pdf')
searchk_df %>% 
  ggplot() +
  geom_line(aes(x = K, y = heldout)) +
  geom_point(aes(x = K, y = heldout)) +
  labs(x = '\nTopics', y = 'Held-Out Likelihood\n') +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 23)) 
dev.off()
  
# Residual
pdf('~/Documents/thesis/data/figures/searchk_residual.pdf')
searchk_df %>% 
  ggplot() +
  geom_line(aes(x = K, y = residual)) +
  geom_point(aes(x = K, y = residual)) +
  labs(x = '\nTopics', y = 'Residuals\n') +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 23))
dev.off()

# Semantic coherence
pdf('~/Documents/thesis/data/figures/searchk_coherence.pdf')
searchk_df %>% 
  ggplot() +
    geom_line(aes(x = K, y = semcoh)) +
    geom_point(aes(x = K, y = semcoh)) +
    labs(x = '\nTopics', y = 'Semantic Coherence\n') +
    theme(axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(size = 23)) 
dev.off()

# Lower bound
pdf('~/Documents/thesis/data/figures/searchk_bound.pdf')
searchk_df %>% 
  ggplot() +
  geom_line(aes(x = K, y = lbound)) +
  geom_point(aes(x = K, y = lbound)) +
  labs(x = '\nTopics', y = 'Lower Bound\n') +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 23)) 
dev.off()


