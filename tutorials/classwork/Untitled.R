set
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
         "lubridate",
         'word2vec',
         'uwot'# working with dates
), pkgTest)


setwd(getwd())

text_csv <- breitbart_2016_sample
text_csv$text <- gsub("<.*?>", "", text_csv$text) # remove html tags using regex
text_csv$text <- gsub("advertisementvar data.*?ba=0;ba", "", text_csv$text) # remove javascript junk

corpus <- corpus(text_csv) # create corpus object

tokens <- tokens(corpus,
                 remove_numbers = TRUE,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_hyphens = TRUE,
                 remove_separators = TRUE,
                 remove_url = TRUE) # create tokens object

stop_list <- stopwords("english") 
tokens <- tokens_remove(tokens, stop_list) # remove stop words

docs <- as.list(tokens) # get text of articles
docs <- tolower(docs)

