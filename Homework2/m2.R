#####remvoed line 62


dfm <- dfm_tfidf(dfm) # weight DFM


Cstack_info()

set.seed(2023) # set seed for replicability
tmpdata <- tmpdata[sample(nrow(tmpdata)), ] # randomly order labelled dataset
split <- round(nrow(tmpdata) * 0.05) # determine cutoff point of 5% of documents
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
  number = 3,
  repeats = 3,
  classProbs= TRUE, 
  summaryFunction = multiClassSummary,
  selectionFunction = "best", # select the model with the best performance metric
  verboseIter = TRUE
)
saveRDS(train_control,"~/Desktop/QTA_Spring23/Homework2/train_control")

??caret
##NAive bayes
## 4. Naive Bayes classification
# You need to a) Check the parameters for Naive Bayes algorithm
modelLookup(model = "naive_bayes")

#             b) Create a matrix of combinations of parameters to supply to tuneGrid arg of train()
tuneGrid <- expand.grid(laplace = c(0,0.5,1.0),
                        usekernel = c(TRUE, FALSE),
                        adjust=c(0.75, 1, 1.25, 1.5))

tuneGrid

#             c) Set up parallel processing
cl <- makePSOCKcluster(7) # create number of copies of R to run in parallel and communicate over sockets
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

confusionMatrix(reference = as.factor(Test$section_labels), data = pred, mode='everything') # generate confusion matrix

#             i) Finalise the model
nb_final <- train(section_labels ~ ., 
                  data = ldata,  
                  method = "naive_bayes", 
                  trControl = trainControl(method = "none"),
                  tuneGrid = data.frame(nb_train$bestTune))

#             j) Save the model!
saveRDS(nb_final, "data/nb_final")

#             k) If your machine is running slow... read in the model 
#nb_final <- readRDS("data/nb_final")

#             l) Predict from validation set
pred2 <- predict(nb_final, newdata = vdata)
head(pred2) # first few predictions

#             m) Evaluate confusion matrix (because we actually have labels...)
confusionMatrix(reference = as.factor(vdata$section_labels), data = pred2, mode='everything')
