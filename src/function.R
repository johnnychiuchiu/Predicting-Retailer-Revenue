library(dplyr)

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
                              mutate(total_duration = days_first_purchase - days_recent_purchase) %>%
                              select(id, days_recent_purchase, days_first_purchase, total_duration)
  
  ### Frequency
  # number of orders
  df_order_count = df %>% group_by(id) %>% summarise(order_count = n_distinct(ordnum))
  
  ### Monetary
  # average order quantity | average order price
  df_average_monetary = df %>% group_by(id, ordnum) %>% summarise(qty_sum=sum(qty), ord_value=sum(total_price)) %>% 
                                group_by(id) %>% summarise(avg_qty = mean(qty_sum), avg_ord_value= mean(ord_value))
  
  ### Others
  
  ### Merge all the dataframe together
  df_list = list(df_last_purchase_time, df_order_count, df_average_monetary)
  result = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

  ### Keep response variable when doing feature engineer for training data
  df_response = df %>% select(id, logtarg)
  df_response = df_response[!duplicated(df_response), ]
  if(isTrain==TRUE){
    result = merge(result, df_response, by='id',all.x= TRUE)
  }   

  return(result)  
}


