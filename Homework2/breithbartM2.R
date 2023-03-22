
## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Acquire stmBrowser package from github
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("mroberts/stmBrowser",dependencies=TRUE)

lapply(c("tidyverse",
         "quanteda",
         "quanteda.textstats",
         "lubridate",
         "stm",
         "wordcloud",
         "stmBrowser",
         "LDAvis"),
       pkgTest)

setwd(getwd())
dat <- read.csv("~/Desktop/QTA_Spring23/Homework2/breitbart_2016_sample.csv", 
                 stringsAsFactors=FALSE,
                 encoding = "utf-8")
###Question 1 Process text and create DFM
corp <- corpus(dat, 
               docid_field = "title",
               text_field = "content")

prepped_toks <- prep_toks(corp) # basic token cleaning
collocations <- get_coll(prepped_toks) # get collocations
toks <- tokens_compound(prepped_toks, pattern = collocations[collocations$z > 10,]) # replace collocations
toks <- tokens_remove(tokens(toks), "") # let's also remove the whitespace placeholders

toks <- tokens(toks, 
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_separators = TRUE,
               remove_url = TRUE,
) # remove other uninformative text
toks <- tokens_remove(toks, c(),
                      valuetype = "fixed")
# Create dfm and weight using tf/idf
dfm <- dfm(toks) # create DFM

dfm <- dfm_trim(dfm, min_docfreq = 20) # trim DFM
saveRDS(dfm,'~/Desktop/QTA_Spring23/Homework2/dfm')
saveRDS(toks,'~/Desktop/QTA_Spring23/Homework2/toks')
## Question 2 Convert dfm to stm with K= 35
stmdfm <- convert(dfm, to = "stm")
saveRDS(stmdfm,'~/Desktop/QTA_Spring23/Homework2/stmdfm')

# Set k
K <- 35

# Run STM algorithm
modelFit <- stm(documents = stmdfm$documents,
                vocab = stmdfm$vocab,
                K = K,
                #prevalence = ~ title + s(as.numeric(date)),
                #prevalence = ~ source + s(as.numeric(date_month)), 
                data = stmdfm$meta,
                max.em.its = 500,
                init.type = "Spectral",
                seed = 2023,
                verbose = TRUE)
stm

# Save your model!
saveRDS(modelFit, "~/Desktop/QTA_Spring23/Homework2/modelFit")


## 3. Interpret Topic model 
# Inspect most probable terms in each topic
labelTopics(modelFit)
#Highets prob 
#FREX freqiency and exclusivity
#lift >1 means word appears more than expected

# Further interpretation: plotting frequent terms
plot.STM(modelFit, 
         type = "summary", 
         labeltype = "frex", # plot according to FREX metric
         text.cex = 0.7,
         main = "Topic prevalence and top terms")

plot.STM(modelFit, 
         type = "summary", 
         labeltype = "prob", # plot according to probability
         text.cex = 0.7,
         main = "Topic prevalence and top terms")

# Use wordcloud to visualise top terms per topic
cloud(modelFit,
      topic = 14,
      scale = c(3, 0.3),
      max.words = 50)

# Reading documents with high probability topics: the findThoughts() function
#not working for some reason
findThoughts(modelFit,
             texts = dfm@docvars$docname_, # If you include the original corpus text, we could refer to this here
             topics = 14,
             n = 5)#this is the number of headings)
# Reading documents with high probability topics: the findThoughts() function

###Q3 discuss All
summary(modelFit)


##Q4
## 4. Topic validation: predictive validity using time series data
###########part 5 and 6 are working
#     a) Convert metadata to correct format
stmdfm$meta$date<- lubridate::dmy(stmdfm$meta$date)

  
stmdfm$meta$date
typeof(stmdfm$meta$date) 

#     b) Aggregate topic probability by month
agg_theta <- setNames(aggregate(modelFit$theta,
                                by = list(date = stmdfm$meta$date),
                                FUN = mean),
                      c("month", paste("Topic",1:K)))
agg_theta <- pivot_longer(agg_theta, cols = starts_with("T"))

#     c) Plot aggregated theta over time
ggplot(data = agg_theta,
       aes(x = month, y = value, group = name)) +
  geom_smooth(aes(colour = name), se = FALSE) +
  labs(title = "Topic prevalence",
       x = "Month",
       y = "Average monthly topic probability") + 
  theme_minimal()

## 5. Semantic validation (topic correlations)
topic_correlations <- topicCorr(modelFit)
plot.topicCorr(topic_correlations,
               vlabels = seq(1:ncol(modelFit$theta)), # we could change this to a vector of meaningful labels
               vertex.color = "white",
               main = "Topic correlations")

## 6. Topic quality (semantic coherence and exclusivity)
topicQuality(model = modelFit,
             documents = stmdfm$documents,
             xlab = "Semantic Coherence",
             ylab = "Exclusivity",
             labels = 1:ncol(modelFit$theta),
             M = 15)

####Final quesition  requires time series thing

