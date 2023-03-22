# Function for cleaning
prep_toks <- function(text_corpus){
  toks <- tokens(text_corpus,
                 include_docvars = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("english"), padding = TRUE) %>%
    #tokens_remove('[\\p{P}\\p{S}]', valuetype = 'regex', padding = TRUE)
  return(toks)
}

# Function for collocations
get_coll <-function(tokens){
  unsup_col <- textstat_collocations(tokens,
                                     method = "lambda",
                                     size = 2,
                                     min_count = 5,
                                     smoothing = 0.5)
  unsup_col <- unsup_col[order(-unsup_col$count),] # sort detected collocations by count (descending)
  return(unsup_col)
}






## Load packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "guardianapi",
         "quanteda", 
         "lubridate",
         "quanteda.textmodels", 
         "quanteda.textstats", 
         "caret", # For train/test split
         "MLmetrics", # For ML
         "doParallel",
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem",
         "quanteda.textplots"), # For parallel processing
       pkgTest)
###begin
setwd(getwd())
data <- read.csv("~/Desktop/QTA_Spring23/Homework2/yelp_data_small.csv", 
                 stringsAsFactors=TRUE,
                 encoding = "utf-8")




##Question 1
table(data$sentiment)
length(data$sentiment)
prob_p <- 5088/10000
###odds of positve
print(prob_p)

#pretty balanced overall slighlty more positve
typeof(data$text)
data$text <- as.character(data$text)
corp_y <- corpus(data)


toks <- quanteda::tokens(corp_y, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE,
                         remove_numbers = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE)
toks <- tokens_tolower(toks)

stop_list <- stopwords("english") # load English stopwords from quanteda
head(stop_list)  
##remove stopwords
toks <- tokens_remove(toks, stop_list)

# Now we'll stem the words using the tokens_wordstem() function
toks <- tokens_wordstem(toks)

collocations2 <- textstat_collocations(toks, size = 2)
keep_coll_list2 <- collocations2$collocation[1:25]
keep_coll_list2
collocations3 <- textstat_collocations(lemma_toks, size = 3)
keep_coll_list3 <- collocations3$collocation[1:15] 
keep_coll_list3
keep_coll_list2_3 <- c(keep_coll_list2,keep_coll_list2)
keep_coll_list2_3



dfm <- dfm(toks) # create DFM
dfm <- dfm_trim(dfm, min_docfreq = 200) # trim DFM
dfm <- dfm_tfidf(dfm) # weight DFM
