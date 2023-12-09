#!/usr/bin/env RScript
### Taking in args 
library("stopwords")
library(text2vec)
library("pROC")
library(glmnet)

args = commandArgs(trailingOnly=TRUE)
myvocab = scan(args[1], character(), quote = "")
train = read.table(args[2],
                   stringsAsFactors = FALSE,
                   header = TRUE)
test = read.table(args[3],
                  stringsAsFactors = FALSE,
                  header = TRUE)

all_reviews = c(train$review)
all_sentiments = train[['sentiment']]
it_train = itoken(all_reviews,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 2L)))
dtm_train_new = create_dtm(it_train, vectorizer)
set.seed(1)

glmnet_classifier = cv.glmnet(x = dtm_train_new, y = all_sentiments,
                              family = 'binomial',
                              alpha = 0,
                              type.measure = "auc",
                              nfolds = 4,
                              thresh = 1e-3,
                              maxit = 1e3)

it_test = word_tokenizer(tolower(test$review))
it_test = itoken(it_test, ids = test$id)

dtm_test = create_dtm(it_test, vectorizer)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]

preds_df = data.frame(unlist(names(preds)), unlist(preds))
names(preds_df) = c('id', 'prob')

file_path = paste0('mypred.csv')
readr::write_csv(as.data.frame(preds_df), file_path)