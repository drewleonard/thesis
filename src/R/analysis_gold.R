rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load CSV
raw <- read.csv('./csv/FacebookAds.csv')

# Facebook events
# Count: 225
# Spend: 448203.1
# Clicks: 86375
# Impressions: 1363048
count(subset(raw, grepl("/events/", LandingPage) | grepl("/events/", AdText)))
sum(subset(raw, (grepl("/events/", LandingPage) | grepl("/events/", AdText)) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (grepl("/events/", LandingPage) | grepl("/events/", AdText)) & !is.na(Clicks))$Clicks)
sum(subset(raw, (grepl("/events/", LandingPage) | grepl("/events/", AdText)) & !is.na(Impressions))$Impressions)

# Facebook general
# Calculates: 2950 - 225 = 2725
# Spend: 4419043 - 448203 = 3970840
# Clicks: 3612877 - 86375 = 3526502
# Impressions: 36754957 - 1363048 = 35391909
count(subset(raw, grepl("facebook.com", LandingPage)))
sum(subset(raw, (grepl("facebook.com", LandingPage)) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (grepl("facebook.com", LandingPage)) & !is.na(Clicks))$Clicks)
sum(subset(raw, (grepl("facebook.com", LandingPage)) & !is.na(Impressions))$Impressions)

# Instagram pages
# Count: 120
# Spend: 236952
# Clicks: 11378
# Impressions: 1991071
count(subset(raw, grepl("instagram.com", LandingPage)))
sum(subset(raw, (grepl("instagram.com", LandingPage)) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (grepl("instagram.com", LandingPage)) & !is.na(Clicks))$Clicks)
sum(subset(raw, (grepl("instagram.com", LandingPage)) & !is.na(Impressions))$Impressions)

# Get number of IRA-domain urls
# Count: 265
# Spend: 178531
# Clicks: 90416
# Impressions: 1396878
count(subset(raw, grepl("blackmattersus.com", LandingPage) | 
               grepl("dudeers.com", LandingPage) |
               grepl("black4black.com", LandingPage) |
               grepl("donotshoot.us", LandingPage)))
sum(subset(raw, (grepl("blackmattersus.com", LandingPage) | 
                   grepl("dudeers.com", LandingPage) |
                   grepl("black4black.com", LandingPage) |
                   grepl("donotshoot.us", LandingPage)) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (grepl("blackmattersus.com", LandingPage) | 
                   grepl("dudeers.com", LandingPage) |
                   grepl("black4black.com", LandingPage) |
                   grepl("donotshoot.us", LandingPage)) & !is.na(Clicks))$Clicks)
sum(subset(raw, (grepl("blackmattersus.com", LandingPage) | 
                   grepl("dudeers.com", LandingPage) |
                   grepl("black4black.com", LandingPage) |
                   grepl("donotshoot.us", LandingPage)) & !is.na(Impressions))$Impressions)

# Non missing pages (totals)
# Count: 3488
# Spend: 4893046
# Clicks: 3729406
# Impressions: 40326539
count(subset(raw, LandingPage != ""))
sum(subset(raw, LandingPage != "" & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, LandingPage != "" & !is.na(Clicks))$Clicks)
sum(subset(raw, LandingPage != "" & !is.na(Impressions))$Impressions)

# External sites
# Count: 
# Spend: 4893046 - 3970840 - 448203 - 236952 - 178531 = 58520
# Clicks: 3729406 - 3526502 - 86375 - 11378 - 90416 = 14735
# Impressions: 40326539 - 35391909 - 1363048 - 1991071 - 1396878 = 183633

# Missing pages
# Count: 28
count(subset(raw, LandingPage != ''))


