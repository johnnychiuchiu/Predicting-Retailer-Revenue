library(dplyr)
library(zoo)
library(ggplot2)
library(car)
library(MASS)
library(glmnet)
library(pROC)
options(scipen=999) # remove scientific notation in printing

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

replace_outlier <- function(df, name, sd_cutoff){
  df_filtered = rm_outlier(df, name, sd_cutoff)
  
  df[,name] = ifelse(df[,name]>sd_cutoff, max(df_filtered[,name]), df[,name])
  return(df)
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
                              mutate(total_duration = days_first_purchase - days_recent_purchase) %>%
                              dplyr::select(id, days_recent_purchase, days_first_purchase, total_duration)
  
  ### Frequency
  # number of orders
  df_order_count = df %>% group_by(id) %>% summarise(order_count = n_distinct(ordnum))
  
  ### Monetary
  # average order quantity | average order price
  df_average_monetary = df %>% group_by(id, ordnum) %>% summarise(qty_sum=sum(qty), ord_value=sum(total_price)) %>% 
                                group_by(id) %>% summarise(avg_qty = mean(qty_sum), avg_ord_value= mean(ord_value))
  # total money spent
  df_total_money = df %>% group_by(id, ordnum) %>% mutate(ord_value=total_price*qty) %>% 
    group_by(id) %>% summarise(total_money = sum(ord_value))
  
  ### Diversity
  # number of categories purcahsed
  df_cat_count = df %>% group_by(id) %>% summarise(cat_count = n_distinct(category))
  
  ### Enthrophy
  # how diverse the amount of purchase are from all categories (~0.5 means diverse)
  df_entrophy = df %>% group_by(id,category) %>% summarise(t_qty = sum(qty)) %>% 
    mutate(freq = t_qty / sum(t_qty)) %>% group_by(id) %>% 
    summarise(entrophy = -sum(freq*log(freq)))
  
  ###Categories per order:
  df_average_catcount = df %>% group_by(id, ordnum) %>% summarise(cat_count_ord=n_distinct(category)) %>% 
    group_by(id) %>% summarise(avg_cats = mean(cat_count_ord))
  
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

  # calculate the Coefficient of variation using the qty over year 
  # reference: https://en.wikipedia.org/wiki/Coefficient_of_variation
  temp = df %>% group_by(id,orddate) %>% summarise(qty=sum(qty)) %>% arrange(id, orddate)
  temp$year = format(temp$orddate,'%Y')
  temp2 = temp %>% group_by(id, year) %>% summarise(qty=sum(qty))
  df_coeva = temp %>% group_by(id) %>% summarise(qty_sd = sd(qty), qty_mean = mean(qty))
  df_coeva[is.na(df_coeva)]=0
  df_coeva$coe_va = df_coeva$qty_sd/df_coeva$qty_mean
  df_coeva = df_coeva %>% dplyr::select(id, coe_va)
  
  # count distinct order date per id
  df_distinct_orddate = df %>% group_by(id) %>% summarise(distinct_orddate = length(unique(orddate)))

  ### after aug
  df_after_aug = df %>% group_by(id) %>% mutate(month = substr(orddate, 6, 7)) %>% summarise(aug_after_orders = sum(as.numeric(month)>7))
  df_aug = df %>% group_by(id) %>% mutate(month = substr(orddate, 6, 7)) %>% summarise(aug_orders = sum(as.numeric(month)==8))
  
  ### Merge all the dataframe together
  df_list = list(df_last_purchase_time, df_order_count, df_average_monetary, 
                 df_slope, df_coeva, df_cat_count, df_entrophy, df_total_money, df_average_catcount, df_distinct_orddate,
                 df_after_aug)
  result = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

  ### Keep response variable when doing feature engineer for training data
  df_response = df %>% dplyr::select(id, logtarg)
  df_response = df_response[!duplicated(df_response), ]
  if(isTrain==TRUE){
    result = merge(result, df_response, by='id',all.x= TRUE)
  }   
  result$ordperyr <- result$order_count / ((result$total_duration/365)+.1)
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