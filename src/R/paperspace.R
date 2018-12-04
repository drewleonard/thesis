# Load packages
library(stm)
library(furrr)

# Settings
plan(multiprocess)

# Read data
df <- read.csv('fb_gold.csv')

# Process data
processed <- textProcessor(documents = df$AdText, metadata = df)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

stm <- stm(out$documents, 
	out$vocab, 
	K = 49, 
	prevalence =~ Interests,
	content =~ Interests, 
	max.em.its = 75, 
	data = out$meta, 
	init.type = "Spectral")

saveRDS(stm, 'interests_content_covariate_model.RDS')