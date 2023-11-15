#!/usr/bin/env RScript
### Taking in args 
library(tidyverse)
library(lubridate)
library(dplyr)
args = commandArgs(trailingOnly=TRUE)
train = read.csv(args[1], stringsAsFactors = FALSE)
test = read.csv(args[2], stringsAsFactors = FALSE)

if (args[2]=='train.csv'){
  train = read.csv(args[2], stringsAsFactors = FALSE)
  test = read.csv(args[1], stringsAsFactors = FALSE)
}



train_and_pred_and_write = function(train, test)
{
  # find the unique pairs of (Store, Dept) combo that appeared in both training and test sets
  train_pairs = train[, c("Store", "Dept")] %>% count(Store, Dept) %>% filter(n != 0)
  test_pairs = test[, c("Store", "Dept")] %>% count(Store, Dept) %>% filter(n != 0)
  unique_pairs = intersect(train_pairs[, c("Store", "Dept")], test_pairs[, c("Store", "Dept")])
  
  # pick out the needed training samples, convert to dummy coding, then put them into a list
  train_split = unique_pairs %>% 
    left_join(train, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date)) %>%
    mutate(Yr2 = year(Date) ^ 2)
  
  
  # mutate(Yr = factor(year(Date)))
  
  test_split = unique_pairs %>% 
    left_join(test, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
    mutate(Yr = year(Date)) %>%
    mutate(Yr2 = year(Date) ^ 2)
  # test_split[test_split$Yr == 2012,] = 2011
  # test_split = test_split %>% mutate(Yr = factor(year(Date)))
  
  # construct the design matrix only once
  train_tibbles = as_tibble(model.matrix(~ Weekly_Sales + Store + Dept + Yr + Yr2+ Wk, train_split)) %>%
    group_split(Store, Dept)
  
  test_tibbles = as_tibble(model.matrix(~ Store + Dept + Yr + Yr2 + Wk, test_split)) %>%
    mutate(Date = test_split$Date) %>% 
    group_split(Store, Dept)
  
  preds = c()
  # perform regression for each split, note we used lm.fit instead of lm
  for (j in 1:nrow(unique_pairs)) 
  {
    tmp_train = train_tibbles[[j]]
    tmp_test = test_tibbles[[j]]
    
    mycoef = lm.fit(as.matrix(tmp_train[, -(2:4)]), tmp_train$Weekly_Sales)$coefficients
    mycoef[is.na(mycoef)] = 0
    tmp_pred = mycoef[1] + as.matrix(tmp_test[, 4:56]) %*% mycoef[-1]
    
    preds = c(preds, tmp_pred)
  }
  
  test_split$Weekly_Pred = preds
  idx_NA = is.na(test_split$Weekly_Pred)
  test_split$Weekly_Pred[idx_NA] = 0
  
  # join back test split to test and fill in NA's, this produces the correct shape we need for WAE later
  mypred_df = test %>% 
    left_join(test_split, by = c('Store', 'Dept', 'Date', 'IsHoliday'))
  
  mypred_df = mypred_df[,c('Store', 'Dept', 'Date', 'IsHoliday', 'Weekly_Pred')]
  
  idx_NA = is.na(mypred_df$Weekly_Pred)
  mypred_df$Weekly_Pred[idx_NA] = 0
  
  for (k in 1:nrow(mypred_df)){
    if(mypred_df$Date[k] == '2011-12-23'){
      mypred_df$Weekly_Pred[ k ] = mypred_df$Weekly_Pred[ k]*6/7
      dept = mypred_df[k,]$Dept
      store = mypred_df[k,]$Store
      date = '2011-12-30'
      spec_pred = mypred_df[mypred_df$'Dept' == dept & mypred_df$'Store' == store & mypred_df$'Date' == date,]$Weekly_Pred 
      mypred_df[mypred_df$'Dept' == dept & mypred_df$'Store' == store & mypred_df$'Date' == date,]$Weekly_Pred  = spec_pred +mypred_df$Weekly_Pred[ k ]*1/7
    }
  }
  file_path = paste0('mypred.csv')
  readr::write_csv(mypred_df, file_path)
}

preprocess.svd <- function(train, n.comp){
  # Replaces the training data with a rank-reduced approximation of itself.
  # This is for noise reduction. The intuition is that characteristics
  # that are common across stores (within the same department) are probably
  # signal, while those that are unique to one store may be noise.
  #
  # args:
  # train - A matrix of Weekly_Sales values from the training set of dimension
  #         (number of weeeks in training data) x (number of stores)
  # n.comp - the number of components to keep in the singular value
  #         decomposition
  #
  # returns:
  #  the rank-reduced approximation of the training data
  
  result <- data.frame(matrix(ncol = 4, nrow = 0))
  result_cols <- c("Dept", "Store", "Date", "Weekly_Sales")
  colnames(result) <- result_cols
  
  for (i in sort(unique(train$Dept))) {
    train_spread = train %>%
      filter(Dept == i) %>%
      select(Store, Date, Weekly_Sales) %>%
      spread(Date, Weekly_Sales)
    
    train_spread[is.na(train_spread)] <- 0
    
    if (dim(train_spread)[1] <= n.comp) { # skip svd
      tg = gather(train_spread, "Date", "Weekly_Sales", -1)
      tg$Dept = i
      tg$Store = as.numeric(tg$Store)
      result = rbind(result, tg)
      next
    }
    z <- svd(train_spread[, 2:ncol(train_spread)], nu=n.comp, nv=n.comp)
    
    u2 = z$u[, 1:n.comp]
    d2 <- diag(z$d[1:n.comp])
    v2 = z$v[, 1:n.comp]
    
    train_spread[, 2:ncol(train_spread)] <- u2 %*% d2 %*% t(v2)
    train_g = gather(train_spread, "Date", "Weekly_Sales", -1)
    train_g$Dept = i
    train_g$Store = as.numeric(train_g$Store)
    result = rbind(result, train_g)
  }
  
  result
}



# file_path = paste0('train.csv')
# train = read.csv(file_path)
# 
# file_path = paste0('test.csv')
# test = read.csv(file_path)

train_smoothed = preprocess.svd(train, 8)

train_and_pred_and_write(train_smoothed, test)


