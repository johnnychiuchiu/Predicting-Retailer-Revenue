library(dplyr)
library(zoo)
library(ggplot2)
library(car)
library(MASS)
library(glmnet)
library(pROC)
library(DAAG)

check_outlier <- function(df, name, sd_cutoff){
  # Using standard deviation to check the outlier for numerical columns
  #
  # Parameters
  # ----------
  # df: data.frame
  # name: chr
  #    the column we want to check for outliers
  # sd_cutoff: num
  #    a number indicating how many standard deviation away from the mean
  # @return: None
  
  mean <- mean(df[,name])
  print(paste('Mean of',name,':',mean))
  sd <- sd(df[,name])
  print(paste('Sd of',name,':',sd))
  condition = paste(name, '<', mean , '-' , sd_cutoff, '*', sd, '|', 
                    name, '>', mean , '+' , sd_cutoff, '*', sd, sep='' )
  View(df %>% filter_(condition))
}

rm_outlier <- function(df, name, sd_cutoff){
  # Remove rows that is outlier defined by standard deviation away from the mean
  #
  # Parameters
  # ----------
  # df: data.frame
  # name: chr
  #    the column we want to check for outliers
  # sd_cutoff: num
  #    a number indicating how many standard deviation away from the mean
  # @return: data.frame
  #    a dataframe removing outliers according to the input condition 
  
  mean <- mean(df[,name])
  sd <- sd(df[,name])
  condition = paste(name, '>=', mean , '-' , sd_cutoff, '*', sd, '&', 
                    name, '<=', mean , '+' , sd_cutoff, '*', sd, sep='' )
  return(df %>% filter_(condition))
}

data_manipulate <- function(df){
  # A function that does several data preprocessing
  #
  # Parameters
  # ----------
  # df: data.frame
  # @return: data.frame
  #    a processed dataframe 
  
  # transform date related column into date format
  df$orddate = as.Date(df$orddate, "%d%b%Y")
  
  # calculate total value for each row
  df = df %>% mutate(total_price = qty*price)
  
  return(df)
}

feature_engineer <- function(df, isTrain=TRUE){
  # A function that output a dataframe with all the created features.
  #
  # Parameters
  # ----------
  # df: data.frame
  # isTrain: bol
  #     indicate whether to include response variabel in the output data.frame or not
  # @return: data.frame
  #    a dataframe that is ready for model building 
  
  ### Recency 
  # time since last purchase | time since first purchase | total duration
  offer_date = as.Date('01AUG2014',"%d%b%Y")
  df_last_purchase_time = df %>% group_by(id) %>% summarise(max_date = max(orddate), min_date = min(orddate)) %>% 
                              mutate(days_recent_purchase = as.integer(offer_date-max_date),
                                     days_first_purchase = as.integer(offer_date-min_date)) %>%
                              dplyr::select(id, days_recent_purchase, days_first_purchase)
  
  ### Frequency
  # number of orders
  df_order_count = df %>% group_by(id) %>% summarise(order_count = n_distinct(ordnum))
  
  # # qty sum of each category for each id -> Using it generate worse result for testset on Kaggle, even though it increase the R^2 in train a lot. Can not verify it using cv.lm either. Just remove it for now.
  # df_temp = df %>% group_by(id, category) %>% summarise(qty=sum(qty))
  # df_category_qty_count = df_temp %>% spread(category, qty)
  # df_category_qty_count[is.na(df_category_qty_count)]=0
  
  ### Monetary
  # average order quantity | average order price
  df_average_monetary = df %>% group_by(id, ordnum) %>% summarise(qty_sum=sum(qty), ord_value=sum(total_price)) %>% 
                                group_by(id) %>% summarise(avg_qty = mean(qty_sum), avg_ord_value= mean(ord_value))
  
  ### Others
  # build a linear regression and use the slope as the trend
  df_reg = df %>% group_by(id,orddate) %>% summarise(qty=sum(qty)) %>% arrange(id, orddate)
  df_slope = df_reg %>% group_by(id) %>% summarise(slope=n())
  for(i in 1:dim(df_slope)[1]){
    if(df_slope$slope[i]!=1){
      df_id_filter = df_reg[df_reg$id == df_slope$id[i],]
      fit_id = lm(qty~orddate, data=df_id_filter)
      df_slope$slope[i] = summary(fit_id)$coefficients[2]
    }else{
      df_slope$slope[i]=0
    }
  }
  df_slope$slope = scale(df_slope$slope)
  df_slope$slope = ifelse(df_slope$slope>=3, 3, df_slope$slope)
  df_slope$slope = ifelse(df_slope$slope<=-3, -3, df_slope$slope)

  # Using monthly unique ordnum count. It doesn't work, since the distribution is too skewed.
  # temp = df %>% mutate(ym = as.yearmon(format(df$orddate,'%Y-%m')))
  # df_reg = temp %>% group_by(id,ym) %>% summarise(ordnum=n_distinct(ordnum)) %>% arrange(id, ym)
  # df_slope = df_reg %>% group_by(id) %>% summarise(slope=n())
  # for(i in 1:dim(df_slope)[1]){
  #   if(df_slope$slope[i]!=1){
  #     df_id_filter = df_reg[df_reg$id == df_slope$id[i],]
  #     fit_id = lm(ordnum~ym, data=df_id_filter)
  #     df_slope$slope[i] = summary(fit_id)$coefficients[2]
  #   }else{
  #     df_slope$slope[i]=0
  #   }
  # }
  # df_slope$slope = scale(df_slope$slope)
  # df_slope$slope = ifelse(df_slope$slope>=3, 3, df_slope$slope)
  # df_slope$slope = ifelse(df_slope$slope<=-3, -3, df_slope$slope)
  
  # calculate the Coefficient of variation using the qty over year 
  # reference: https://en.wikipedia.org/wiki/Coefficient_of_variation
  temp = df %>% group_by(id,orddate) %>% summarise(qty=sum(qty)) %>% arrange(id, orddate)
  temp$year = format(temp$orddate,'%Y')
  temp2 = temp %>% group_by(id, year) %>% summarise(qty=sum(qty))
  df_coeva = temp %>% group_by(id) %>% summarise(qty_sd = sd(qty), qty_mean = mean(qty))
  df_coeva[is.na(df_coeva)]=0
  df_coeva$coe_va = df_coeva$qty_sd/df_coeva$qty_mean
  df_coeva = df_coeva %>% dplyr::select(id, coe_va)
  
  
  ### Merge all the dataframe together
  df_list = list(df_last_purchase_time, df_order_count, df_average_monetary, 
                 df_slope, df_coeva)#df_category_qty_count
  result = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

  ### Keep response variable when doing feature engineer for training data
  df_response = df %>% dplyr::select(id, logtarg)
  df_response = df_response[!duplicated(df_response), ]
  if(isTrain==TRUE){
    result = merge(result, df_response, by='id',all.x= TRUE)
  }   

  return(result)  
}

my_cv_glmnet <- function(y, x, alpha){
  # set seed for cross validation
  set.seed(1)
  
  # using cv.glmnet to build lasso. The following line calculate 3 fold cv for each lambda, so there will be 1000*3 model fitting.
  fit.cv=cv.glmnet(x,y,alpha=alpha,nfold=3,lambda=seq(0,10,0.01))
  
  # get the lambda with the smallest Mean-Squared Error
  fitted_min_lambda=fit.cv$lambda.min
  
  # get the index of the smallest lambda, and use it to find our ideal coefficient
  small.lambda.index <- which(fit.cv$lambda == fit.cv$lambda.min)
  small.lambda.betas <- coef(fit.cv$glmnet.fit)[,small.lambda.index]
  
  return(list(lambda=fitted_min_lambda,
              small.lambda.betas=small.lambda.betas))
}

get_optimal_p <- function(real_response, predict_fit, p_threshold_list){
  # get the optimal probability threshold according to CCR / f1_score
  #
  # Parameters
  # ----------
  # @param real_response: actual response vector
  # @param predict_fit: predicted probabilty from the model
  # @param p_threshold_list: list of probability that we want to test on
  # @return: optimal probability threshold according to CCR
  
  max_metric= 0
  optimal_p = 0
  
  
  for (p in p_threshold_list){
    pred = rep(0, length(real_response))
    pred[predict_fit > p]=1
    pred= factor(pred, levels=c(0,1))
    # ccr = sum(diag(table(real=real_response, pred)))/ length(real_response)
    
    confusion_table = table(real=real_response, pred)
    
    sensitivity=confusion_table[2,2]/(confusion_table[2,1]+confusion_table[2,2])
    specificity=confusion_table[1,1]/(confusion_table[1,1]+confusion_table[1,2])
    precision=confusion_table[2,2]/(confusion_table[1,2]+confusion_table[2,2])
    f1_score=2*precision*sensitivity/(precision+sensitivity)
    # print(paste("f1_score:",f1_score," when p=",p))
    
    if (f1_score >= max_metric){
      max_metric= f1_score
      optimal_p = p
    }
  }
  
  return(optimal_p)
}

calculate_metrics <- function(df,real_response, predict_fit, optimal_p){
  # a function that calculate all the classification related metrics, including confusion table, auc, ccr, ...etc.
  #
  # Parameters
  # ----------  
  # @param real_response: actual response vector
  # @param predict_fit: predicted probabilty from the model
  # @param optimal_p: optimal probability threshold according to CCR
  # @return: a list of auc, ccr, sensitivity, precision, f1_score 
  
  # generate the vecotr that transform the fitted result into a vector of classified numbers
  predict_response = rep(0,dim(df)[1])
  predict_response[predict_fit>optimal_p[1]]=1
  
  
  confusion_table = table(actual = real_response, prediction = predict_response)
  print(confusion_table)
  
  ccr = sum(diag(confusion_table))/ sum(confusion_table)
  sensitivity=confusion_table[2,2]/(confusion_table[2,1]+confusion_table[2,2])
  specificity=confusion_table[1,1]/(confusion_table[1,1]+confusion_table[1,2])
  precision=confusion_table[2,2]/(confusion_table[1,2]+confusion_table[2,2]) 
  f1_score=2*precision*sensitivity/(precision+sensitivity)
  
  plot.roc(real_response, predict_fit, xlab="1-Specificity")
  
  my_auc = auc(real_response, predict_fit)
  
  return(list(auc=my_auc,
              ccr=ccr,
              sensitivity=sensitivity, 
              specificity=specificity,
              f1_score=f1_score))
}

