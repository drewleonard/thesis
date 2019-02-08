# Script adapted / adopted from package documentation

rm(list = ls())
setwd("~/Documents/thesis/data/")

library(texteffect)

# Load data
data(BioSample)

# Divide into training and test sets
Y <- BioSample[,1]
X <- BioSample[,-1]
set.seed(1)
train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)

# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y, K = 2, alphas = c(2,4), sigmasq.ns = c(0.8, 1), 
                                 iters = 1, train.ind = train.ind)

## Not run: 
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)

# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["4"]][["0.8"]][[1]], colnames(X), 10, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["1"]][[1]], colnames(X), 10, verbose = TRUE)

# Select the most interest treatments to investigate
sibp.fit <- sibp.search[["4"]][["0.8"]][[1]]

# Estimate the AMCE using the test set
amce<-sibp_amce(sibp.fit, X, Y)
# Plot 95% confidence intervals for the AMCE of each treatment
sibp_amce_plot(amce)
