pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "guardianapi", # for working with the Guardian's API
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem", # an alternative method for lemmatizing
         "lubridate" # working with dates
), pkgTest)


setwd(getwd())
data <- read.csv("./data/us_tweets.csv", 
                 stringsAsFactors=FALSE,
                 encoding = "utf-8")
#Question 1
glimpse(us_tweets)
length(us_tweets$X)
#899 tweets

#Question 1.1
originals <- us_tweets%>%
  filter(is_retweet == "FALSE")
summary(originals)


length(originals$is_retweet)
#same


#question 1.2
trump <- us_tweets%>%
  filter(screen_name == "realDonaldTrump")
length(trump$X)
#ill come back to it
help('tokens')
trial <- trump%>%
  filter(text == '?')




trumpfix <- quanteda::tokens(trump$text, 
                           include_docvars = TRUE,
                           remove_numbers = TRUE,
                           remove_punct = FALSE,
                           remove_symbols = FALSE,
                           remove_separators = TRUE,
                           remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords




