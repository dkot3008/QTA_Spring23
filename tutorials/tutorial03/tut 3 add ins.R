library(quanteda)
library(dplyr)
library(lubridate)

library(readtext) # for importing external corpus data

# Read in csv file with readtext. Note this address will be different for your system.
kcna_csv <- readtext("./data/kcna_example2015.csv", 
                     encoding = "utf-8",
                     text_field = "newsText") # the column with the header "newsText" in the csv contains the article text.
kcna <- kcna_example2015_csv
# Generate datetimes
kcna_csv$date <- dmy(kcna_csv$newsDate)
kcna_csv$week <- week(kcna_csv$date)

# Create a quanteda corpus using the imported data
corpus_kcna <- corpus(kcna_csv)

# Create a dfm object with processed text
kcna_dfm <- corpus_kcna %>%
  quanteda::tokens(remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_hyphens = TRUE,
                   remove_separators = TRUE,
                   remove_url = TRUE,
                   include_docvars = TRUE) %>%
  dfm()

