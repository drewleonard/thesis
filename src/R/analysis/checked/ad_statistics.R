rm(list = ls())
setwd("~/Documents/thesis/data/")

# Load CSV
raw <- read.csv('./csv/FacebookAds.csv')

# Facebook events
# Count: 225
# Spend: 448203.1
# Clicks: 86375
# Impressions: 1363048
count(subset(raw, grepl("/events/", LandingPage) |
               grepl("/events/", AdText)))
sum(subset(raw, (
  grepl("/events/", LandingPage) |
    grepl("/events/", AdText)
) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (
  grepl("/events/", LandingPage) |
    grepl("/events/", AdText)
) & !is.na(Clicks))$Clicks)
sum(subset(raw, (
  grepl("/events/", LandingPage) |
    grepl("/events/", AdText)
) & !is.na(Impressions))$Impressions)

# Facebook general
# Calculates: 2951 - 225 = 2726
# Spend: 4420211 - 448203 = 3972008
# Clicks: 3613270 - 86375 = 3526895
# Impressions: 36767610 - 1363048 = 35404562
count(subset(raw, grepl("facebook", LandingPage)))
sum(subset(raw, (grepl(
  "facebook", LandingPage
)) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (grepl(
  "facebook", LandingPage
)) & !is.na(Clicks))$Clicks)
sum(subset(raw, (grepl(
  "facebook", LandingPage
)) & !is.na(Impressions))$Impressions)

# Instagram pages
# Count: 120
# Spend: 236952
# Clicks: 11378
# Impressions: 1991071
count(subset(raw, grepl("instagram", LandingPage)))
sum(subset(raw, (grepl(
  "instagram", LandingPage
)) & !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, (grepl(
  "instagram", LandingPage
)) & !is.na(Clicks))$Clicks)
sum(subset(raw, (grepl(
  "instagram", LandingPage
)) & !is.na(Impressions))$Impressions)

# Get number of IRA-domain urls
# Count: 125
# Spend: 32488.71
# Clicks: 3006
# Impressions: 166478
count(subset(
  raw,
  grepl("blackmatters.us", LandingPage) |
    grepl("dudeers.com", LandingPage) |
    grepl("black4black.info", LandingPage) |
    grepl("donotshoot.us", LandingPage) |
    grepl("hilltendo.com", LandingPage) |
    grepl("musicfb.info", LandingPage)
))
sum(subset(
  raw,
  (
    grepl("blackmatters.us", LandingPage) |
      grepl("dudeers.com", LandingPage) |
      grepl("black4black.info", LandingPage) |
      grepl("donotshoot.us", LandingPage) |
      grepl("hilltendo.com", LandingPage) |
      grepl("musicfb.info", LandingPage)
  ) & !is.na(AdSpend) & AdSpendCurrency == 'RUB'
)$AdSpend)
sum(subset(raw, (
  grepl("blackmatters.us", LandingPage) |
    grepl("dudeers.com", LandingPage) |
    grepl("black4black.info", LandingPage) |
    grepl("donotshoot.us", LandingPage) |
    grepl("hilltendo.com", LandingPage) |
    grepl("musicfb.info", LandingPage)
) & !is.na(Clicks))$Clicks)
sum(subset(raw, (
  grepl("blackmatters.us", LandingPage) |
    grepl("dudeers.com", LandingPage) |
    grepl("black4black.info", LandingPage) |
    grepl("donotshoot.us", LandingPage) |
    grepl("hilltendo.com", LandingPage) |
    grepl("musicfb.info", LandingPage)
) & !is.na(Impressions))$Impressions)

# Non missing pages (totals)
# Count: 3488
# Spend: 4893046
# Clicks: 3729406
# Impressions: 40326539
count(subset(raw, LandingPage != ""))
sum(subset(raw, LandingPage != "" &
             !is.na(AdSpend) & AdSpendCurrency == 'RUB')$AdSpend)
sum(subset(raw, LandingPage != "" & !is.na(Clicks))$Clicks)
sum(subset(raw, LandingPage != "" &
             !is.na(Impressions))$Impressions)

# External sites
# Count: 3488 - 2725 - 225 - 120 - 125 = 293
# Spend: 4893046 - 3970840 - 448203 - 236952 - 32488 = 204563
# Clicks: 3729406 - 3526502 - 86375 - 11378 - 3006 = 102145
# Impressions: 40326539 - 35391909 - 1363048 - 1991071 - 166478 = 1414033

# Missing pages
# Count: 28
count(subset(raw, LandingPage != ''))
