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
#899-633

length(originals$is_retweet)
#633


#question 1.2
trump <- us_tweets%>%
  filter(screen_name == "realDonaldTrump")
length(trump$X)
#600
##corpus made
tcorp <- corpus(trump, 
                 docid_field = "X",
                 text_field = "text")

sumtcorp <- summary(tcorp, 
                     n = nrow(docvars(tcorp))) #note: the default is n=100
#tokenise
trumptok <- quanteda::tokens(tcorp, 
                             include_docvars = TRUE,
                             remove_numbers = TRUE,
                             remove_punct = FALSE,
                             remove_symbols = FALSE,
                             remove_separators = TRUE,
                             remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
##frequency
tokens_wordstem(trumptok)
t_dft <- dfm(trumptok)
tfreq <- textstat_frequency(dft, n = 1000)
##frequency of "!"
tfreq%>% 
  filter(feature== "!")

#frequency of "win
tfreq%>% 
  filter(feature == "win")

#freqeuncy of employment 
tfreq%>% 
  filter(feature == "employment")

#frequency of immigration
tfreq%>% 
  filter(feature == "immigration")
#6 hoax
tfreq%>% 
  filter(feature == "hoax")
############2.1
###frequency for all
unique(us_tweets$screen_name)
###trump
ttok <- quanteda::tokens(tcorp, 
                             include_docvars = TRUE,
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_separators = TRUE,
                             remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
tokens_wordstem(ttok)
t_dft <- dfm(ttok)
tfreq <- textstat_frequency(t_dft, n = 30)
###rudy
rudy <- us_tweets%>%
  filter(screen_name == "RudyGiuliani")


rcorp <- corpus(rudy, 
                docid_field = "X",
                text_field = "text")

sumrcorp <- summary(rcorp, 
                    n = nrow(docvars(rcorp))) #note: the default is n=100
#tokenise
rtok <- quanteda::tokens(rcorp, 
                             include_docvars = TRUE,
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_separators = TRUE,
                             remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
##frequency
tokens_wordstem(rtok)
r_dft <- dfm(rtok)
rfreq <- textstat_frequency(r_dft, n = 30)
#############pelosi
pelosi <- us_tweets%>%
  filter(screen_name == "SpeakerPelosi")
##corpus 
pcorp <- corpus(pelosi, 
                docid_field = "X",
                text_field = "text")

sumpcorp <- summary(pcorp, 
                    n = nrow(docvars(pcorp))) #note: the default is n=100
#tokenise
ptok <- quanteda::tokens(pcorp, 
                         include_docvars = TRUE,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
##frequency
tokens_wordstem(ptok)
p_dft <- dfm(ptok)
pfreq <- textstat_frequency(p_dft, n = 30)

#####schiff
schiff <- us_tweets%>%
  filter(screen_name == "RepAdamSchiff")

scorp <- corpus(schiff, 
                docid_field = "X",
                text_field = "text")

sumscorp <- summary(scorp, 
                    n = nrow(docvars(scorp))) #note: the default is n=100
#tokenise
stok <- quanteda::tokens(scorp, 
                         include_docvars = TRUE,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
##frequency
tokens_wordstem(stok)
s_dft <- dfm(stok)
sfreq <- textstat_frequency(s_dft, n = 30)
################2.2
#####trump and pelosi
tp <- us_tweets%>%
  filter(screen_name == "SpeakerPelosi" | screen_name =="realDonaldTrump")



tp_corp <- corpus(tp$text, 
                docid_field = "text",
                text_field = "screen_name")

sumtpcorp <- summary(tp_corp, 
                    n = nrow(docvars(tp_corp))) #note: the default is n=100
#tokenise
tp_tok <- quanteda::tokens(tp_corp, 
                         include_docvars = TRUE,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
##frequency
tokens_wordstem(tp_tok)
tp_dft <- dfm(tp_tok)
tpfreq <- textstat_frequency(tp_dft, n = 30)

##now with collocations
colc_tp <- textstat_collocations(tp_tok, size = 2, min_count = 8)



###2.3 keyword in context analysis
help("kwic")
trump_kwic <- kwic(trumptok,
                     pattern = phrase(tfreq$feature),
                     window = 3,
                     case_insensitive = TRUE)

pelosi_kwic <- kwic(ptok,
                   pattern = phrase(pfreq$feature),
                   window = 3,
                   case_insensitive = TRUE)







###2.4 sentiment analysis of Trump’s tweets using the Lexicon Sentiment Dictionary
lubridate to generate a date object variable from the “created_at” variable before plotting. 
For example: docvars(dfm, "date") <- lubridate::ymd_hms(dfm@docvars$created_at)
help("lubridate")
#dfmtrump <- dfm(ttok)
docvars(dfmtrump, "date") <- lubridate::ymd_hms(dfmtrump@docvars$created_at)


dfm_sentiment <- dfm_lookup(dfmtrump, data_dictionary_LSD2015[1:2]) %>%
  dfm_group(groups = date)

# Once we have the frequency for positive and negative sentiment, we can 
# use a useful feature of R - vectorisation - to calculate net sentiment 
# across each day and plot it.
docvars(dfm_sentiment, "prop_negative") <- as.numeric(dfm_sentiment[,1] / ntoken(dfm_sentiment))
docvars(dfm_sentiment, "prop_positive") <- as.numeric(dfm_sentiment[,2] / ntoken(dfm_sentiment))
docvars(dfm_sentiment, "net_sentiment") <- docvars(dfm_sentiment, "prop_positive") - docvars(dfm_sentiment,"prop_negative")

docvars(dfm_sentiment) %>%
  ggplot(aes(x = yday(date), y = net_sentiment, group = year(date))) +
  geom_smooth(aes(colour = as.character(year(date)))) +
  labs(title = "Trump sentiment over time", 
       x = "day of year", y = "net sentiment", 
       colour = "year")

interperation of part 1 outcomes

Trump talks about himself alot with the words president,trump and his username
being in his top words 
Unsurpisingly the other ai pointof interest is things e doesnt like deomcrats and news
Not massively surprising byt disapointing

Rudy loves to talk baout scandels and issues with his top word

pelosi lots fo trump and interaction with him being attempted along with a notably 
the words act and fight

schiff tbc


















###total corpus attempt
corp <- corpus(us_tweets, 
                docid_field = "X",
                text_field = "text")
sumcorp <- summary(corp, 
                    n = nrow(docvars(corp)))

corptok <- quanteda::tokens(corp, 
                             include_docvars = TRUE,
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_separators = TRUE,
                             remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords
##frequency
tokens_wordstem(corptok)
corp_dft <- dfm(corptok)
corp_freq <- textstat_frequency(corp_dft, n = 500)

###failed attempts
help('tokens')
winning <- trumptok%>%
  filter(text == 'winning')



trumptok <- quanteda::tokens(trump$text, 
                             include_docvars = TRUE,
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = FALSE,
                             remove_separators = TRUE,
                             remove_url = TRUE) %>% #Create tokens object
  tokens_tolower() %>% # Transform to lower case
  tokens_remove(stopwords("english")) # Remove stopwords

tokens_lookup(trumptok ,valuetype = c('win'), 
              case_insensitive = TRUE)
tokens_lookup
typeof(trumptok)
help("quanteda")
as.list(trumptok)
test <-as.character(trumptok)
(test)
summary(corpus(test))
typeof(corpus(test))
x <- tokenize_character(trumptok)
glimpse(x)
