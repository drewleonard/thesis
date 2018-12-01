rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load packages
library(stm)
library(furrr)

# Settings
plan(multiprocess)

# Load and format data
fb <- read.csv("./csv/fb_meta_cleaned.csv")
fb <- subset(fb, AdText != "?????? ??? ????? ? ??????????")

# Format date
fb$"CreationDate" <- as.Date(strptime(fb$CreationDate, "%m/%d/%y %H:%M:%S"))
fb$"CreationDateInteger" <- as.integer(format(as.Date(fb$CreationDate), "%m%d%y"))

# Format and bin age
getMidAge <- function(string){
  # Works with ages formatted e.g.,:
  # "15 - 31"
  # "15 - 65+"
  sum(as.integer(unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))))/2
}
fb$Age <- apply(fb[,c('Age'),drop=F], 1, function(x) getMidAge(x) )
fb$BinAge <- factor(Hmisc::cut2(fb$Age, g = 4), labels = c('low', 'mid','high'))

# Bin AdSpend, Clicks, Impressions
fb$BinAdSpend <- factor(Hmisc::cut2(fb$AdSpend, g = 4), labels = c('unavailable', 'low', 'mid','high'))
fb$BinClicks <- factor(Hmisc::cut2(fb$Clicks, g = 4), labels = c('unavailable', 'low', 'mid','high'))
fb$BinImpressions <- factor(Hmisc::cut2(fb$Impressions, g = 4), labels = c('unavailable', 'low', 'mid','high'))

# Save cleaned and formatted dataframe used in preprocessing, model
#write.csv(fb, "./csv/post_fb_meta_cleaned.CSV")

# Process data
processed <- textProcessor(documents = fb$AdText, metadata = fb)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

searchK_results <- searchK(out$documents, 
                           out$vocab, 
                           K = c(20,30,40,50,60), 
                           prevalence =~ Interests, 
                           data = out$meta,
                           init.type = "Spectral")
saveRDS(searchK_results, "./rds/searchk_interests_50.RDS")
plot(searchK_results)

stm_49 <- stm(out$documents, 
              out$vocab, 
              K = 49,
              prevalence =~ Interests + s(Age),
              data = out$meta,
              init.type = "Spectral")
saveRDS(stm_49, "./rds/interests_age_49.RDS")

stm_49_2 <- stm(out$documents, 
              out$vocab, 
              K = 49,
              prevalence =~ Interests + BinAge + BinAdSpend,
              data = out$meta,
              init.type = "Spectral")

