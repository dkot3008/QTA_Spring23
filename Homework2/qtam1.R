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
####Question 2 Make a DFM
typeof(data$text)

data$text <- as.character(data$text)
corp_y <- corpus(data)
##tokenisation of the corpus
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
stem_toks <- tokens_wordstem(toks)

toks_list <- as.list(toks) 
#lematization
lemma_toks <- lapply(toks_list, lemmatize_words) 
lemma_toks <- as.tokens(lemma_toks) 

#rerun from lemmatoks
lemma_toks <- tokens_remove(lemma_toks, c("n"),
                        valuetype = "fixed")
saveRDS(lemma_toks,"~/Desktop/QTA_Spring23/Homework2/lemma_toks")
saveRDS(keep_coll_list2_3,"~/Desktop/QTA_Spring23/Homework2/keep_coll_list2_3")
saveRDS(nb_train, "~/Desktop/QTA_Spring23/Homework2/nb_train")

##The N is a document formating thing and is to be removed

##words appearong toegther
collocations2 <- textstat_collocations(lemma_toks, size = 2)
keep_coll_list2 <- collocations2$collocation[1:25]
keep_coll_list2
collocations3 <- textstat_collocations(lemma_toks, size = 3)
keep_coll_list3 <- collocations3$collocation[1:15] 
keep_coll_list3
keep_coll_list2_3 <- c(keep_coll_list2,keep_coll_list2)
keep_coll_list2_3
#collocations4 <- textstat_collocations(lemma_toks, size = 4)
#above 3 it doesnt make a whole loy of senese some at 3 make sense and add to the dfm
comp_tok <- tokens_compound(lemma_toks, keep_coll_list2_3)



dfm_y <- dfm(comp_tok)
##top features
topfeatures(dfm_y)
##The N is a document formating thing and is to be removed
#rerun from lemmatoks
lemma_toks <- tokens_remove(lemma_toks, c("n"),
  valuetype = "fixed")

comp_tok <- tokens_remove(comp_tok, c("n",
                                          'get',
                                          'time',
                                          'one',
                                          'just',
                                          'come',
                                          'say'),
                            valuetype = "fixed")


dfm_y %>%
  dfm_trim(min_termfreq = 3) %>%
  textplot_wordcloud(min_size = 1, max_size = 10, max_words = 100)
#adjustemnt testing issue
dfm_y <- dfm_trim(dfm_y, min_docfreq = 1500) # trim DFM
dfm_y <- dfm_tfidf(dfm_y) # weight DFM

tmpdata <- convert(dfm_y, to = "data.frame", docvars = NULL)
tmpdata <- tmpdata[, -1] # drop document id variable (first variable)
#sentiment_labels <- dfm_y@docvars$sentiment # get section labels - note, the @ operator is specific to S4 class object
sentiment_labels <- dfm_y@docvars$docname_ # get section labels - note, the @ operator is specific to S4 class object
tmpdata <- as.data.frame(cbind(sentiment_labels, tmpdata)) # labelled data frame
##pointless words get,time,one,just

#question 3 caret
set.seed(2023) # set seed for replicability
tmpdata <- tmpdata[sample(nrow(tmpdata)), ] # randomly order labelled dataset
split <- round(nrow(tmpdata) * 0.05) # determine cutoff point of 5% of documents
#split <- round(nrow(tmpdata) * 0.01) # determine cutoff point of 5% of documents
vdata <- tmpdata[1:split, ] # validation set
ldata <- tmpdata[(split + 1):nrow(tmpdata), ] # labelled dataset minus validation set

#             b) Create an 80/20 test/train split
train_row_nums <- createDataPartition(ldata$sentiment, 
                                      p=0.8, 
                                      list=FALSE) # set human_labels as the Y variable in caret
Train <- ldata[train_row_nums, ] # training set
Test <- ldata[-train_row_nums, ] # testing set
##The og stack overflow lol
'ulimit -s unlimited'
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs= TRUE, 
  summaryFunction = multiClassSummary,
  selectionFunction = "best", # select the model with the best performance metric
  verboseIter = TRUE
)

## 4. Naive Bayes classification
# You need to a) Check the parameters for Naive Bayes algorithm
modelLookup(model = "naive_bayes")

#             b) Create a matrix of combinations of parameters to supply to tuneGrid arg of train()
tuneGrid <- expand.grid(laplace = c(0,0.5,1.0),
                        usekernel = c(TRUE, FALSE),
                        adjust=c(0.75, 1, 1.25, 1.5))

tuneGrid

#             c) Set up parallel processing
cl <- makePSOCKcluster(8) # create number of copies of R to run in parallel and communicate over sockets
# Note that the number of clusters depends on how many cores your machine has.  
registerDoParallel(cl) # register parallel backed with foreach package

#             d) Train the model
'ulimit -s unlimited'
#'memory.limit(size=)'

nb_train <- train(sentiment_labels ~ ., 
                  data = Train,  
                  method = "naive_bayes", 
                  metric = "F1",
                  trControl = train_control,
                  tuneGrid = tuneGrid,
                  allowParallel= TRUE
)

??caret
#############error to many varibales
#             e) Save the model!
##################################Eva iintructions
###check if the Stop sign is there 
###If its gone click on the line below 
####Then click the run button 
saveRDS(nb_train, "~/Desktop/QTA_Spring23/Homework2/nb_train")

#             f) If your machine is running slow... read in the model
#nb_train <- readRDS("data/nb_train")

#             g) Stop the cluster
stopCluster(cl) # stop parallel process once job is done

#             h) Evaluate performance
print(nb_train) # print cross-validation results
pred <- predict(nb_train, newdata = Test) # generate prediction on Test set using training set model
head(pred) # first few predictions

confusionMatrix(reference = as.factor(Test$sentiment_labels), data = pred, mode='everything') # generate confusion matrix

#             i) Finalise the model
nb_final <- train(sentiment_labels ~ ., 
                  data = ldata,  
                  method = "naive_bayes", 
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(nb_train$bestTune))

#             j) Save the model!
saveRDS(nb_final, "~/Desktop/QTA_Spring23/Homework2/nb_final")

#             k) If your machine is running slow... read in the model 
#nb_final <- readRDS("data/nb_final")

#             l) Predict from validation set
pred2 <- predict(nb_final, newdata = vdata)
head(pred2) # first few predictions

#             m) Evaluate confusion matrix (because we actually have labels...)
confusionMatrix(reference = as.factor(vdata$section_labels), data = pred2, mode='everything')



## 4. Training a Support Vector Machine
# You need to a) Examine parameters 
modelLookup(model = "svmLinear")

#             b) Create a grid
tuneGrid <- expand.grid(C = c(0.5, 1, 1.5))

#             c) Set up parallel processing
cl <- makePSOCKcluster(8) # using 6 clusters. 
registerDoParallel(cl)

#             d) Train the model
svm_train <- train(sentiment_labels ~ ., 
                   data = Train,  
                   method = "svmLinear", 
                   metric = "F1",
                   trControl = train_control,
                   tuneGrid = tuneGrid,
                   allowParallel= TRUE
)

#             e) Save the model!
saveRDS(svm_train, "~/Desktop/QTA_Spring23/Homework2/svm_train")

#             f) If your machine is running slow... read in the model
#svm_train <- readRDS("data/svm_train") 

#             g) Stop the cluster
stopCluster(cl)

#             h) Evaluate performance
print(svm_train)
pred_svm <- predict(svm_train, newdata = Test) # Predict on test sample using best model
confusionMatrix(reference = as.factor(Test$sentiment_labels), data = pred_svm, mode='everything')

#             i) Finalise by training on all labelled data
svm_final <- train(sentiment_labels ~ ., 
                   data = ldata,  
                   method = "svmLinear", 
                   trControl = trainControl(method = "none"),
                   tuneGrid = data.frame(svm_train$bestTune))
print(svm_final)

#             j) Save the model!
saveRDS(svm_final, "~/Desktop/QTA_Spring23/Homework2/svm_final")

#             k) In case your computer is running slow... read in the model
#svm_final <- readRDS("data/svm_final")

#             l) Predict from validation set
svm_pred2 <- predict(svm_final, newdata = vdata)

#             m) Evaluate confusion matrix
confusionMatrix(reference = as.factor(vdata$sentiment_labels), data = svm_pred2, mode='everything')



