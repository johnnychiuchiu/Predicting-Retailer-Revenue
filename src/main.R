library(dplyr)
library(ggplot2)
library(car)
library(MASS)
library(glmnet)
library(pROC)
library(DAAG)
options(scipen=999) # remove scientific notation in printing


########## Working directory and source function
pwd = getwd()
source(paste(pwd,'/src/function.R',sep=''))


########## Read Data
booktrain_path = paste(getwd(),'/data/booktrain.csv',sep='')
booktrain = read.csv(booktrain_path)

orders_path = paste(getwd(),'/data/orders.csv',sep='')
orders = read.csv(orders_path)


##### Only keep the rows from orders that can be matched to booktrain, i.e., the rows with logtarg.
all= merge(orders, booktrain, by='id',all.x= TRUE)
all =replace_outlier(all, 'qty', 100)

##### Create Train and Test Dataset
train_original = all %>% filter(!is.na(logtarg)) # note that train data contains only 8224 customer id instead of 8311, since there are 87 ids in booktrain that can not be matched to the records in orders.
train_original = data_manipulate(train_original)

test_original = all %>% filter(is.na(logtarg))
test_original = data_manipulate(test_original)


########## Exploratory Data Analysis
### check NA
colSums(is.na(train_original)) # do not have any NA

### check the distribution for categorical columns
ggplot(data=train_original, aes(x=category)) + geom_histogram(binwidth=1)

### check outlier for numerical columns
# > qty
check_outlier(train_original, 'qty', 10) # I think all the outliers are correct but extreme value. Decide to keep it for now.
check_outlier(train_original, 'price', 10) # I think all the outliers are correct but extreme value. Decide to keep it for now.

# > price
# there are 3.25% of rows with the value 0 in column 'price', and most of them are from category 99 (61%).
View(train_original[train_original$price==0,]) 
table(train_original[train_original$price==0,]$category)/dim(train_original[train_original$price==0,])[1]


########## Feature Engineering
train = feature_engineer(train_original)
test = feature_engineer(test_original, isTrain=FALSE)
system("say Just finished!")


train_reg = train %>% filter(logtarg>0)

########## Model Building Version 1
##### Description: Fit a simple regression model for benchmark
fit_multiple = lm(logtarg~.-id-total_duration, data = train_reg)
summary(fit_multiple)
vif(fit_multiple)

cv.lm(data=train_reg, m=10, form.lm=fit_multiple, seed = 1, plotit=FALSE) # ms 0.375 

##### > Linear Regression with custom chosen variables
chosen_feature = c('avg_qty','slope' ,'coe_va','logtarg')#'days_recent_purchase', 'days_first_purchase','order_count',
fit_custom = lm(logtarg~., data = train_reg[chosen_feature]) 
summary(fit_custom)

cv.lm(data=train[chosen_feature], m=10, form.lm=fit_custom, seed = 1, plotit=FALSE) # ms 0.375

##### > Stepwise Linear Regression
# **Backward**
fit_stepback <- stepAIC(fit_multiple,direction = c("backward"))
summary(fit_stepback) # Adjusted R-squared:  0.01663

cv.lm(data=train, m=10, form.lm=fit_stepback, seed = 1, plotit=FALSE) # ms ms 0.375

# **Forward**
fit_zero <- lm(logtarg ~ 1, data = train_reg)
fit_stepforw <- stepAIC(fit_zero,direction = c("forward"), scope=list(upper=fit_multiple,lower=fit_zero))
summary(fit_stepforw) # Adjusted R-squared:  0.007776

##### > Lasso Linear Regression
y=train_reg$logtarg
x=model.matrix(logtarg~.-id-total_duration,train_reg)
param_lasso = my_cv_glmnet(y,x,1)$small.lambda.betas
param_lasso = param_lasso[param_lasso!=0]
lasso_feature = c('logtarg','avg_qty','slope' ,'coe_va','avg_ord_value')
fit_lasso = lm(logtarg~., data = train_reg[lasso_feature])
summary(fit_lasso) #Adjusted R-squared:  0.01626

cv.lm(data=train[lasso_feature], m=10, form.lm=fit_lasso, seed = 1, plotit=FALSE) 

##### Model selection
# choose the model according to Adjusted R-squared, Feature Significance, cv mean square error. 
# Multiple linear, custom and stepback give us similar result in Adjusted R^2 and cv ms.
# I'll choose fit_multiple as our final model now, since it somehow give us the best test error in my first try.


# ##### Make prediction
# predict_reg = predict(fit_stepback, newdata=test)
# 
# ##### Generating submitting file
# test$logtarg = predict_reg
# file_version1 = test %>% dplyr::select(id, logtarg)
# file_version1$logtarg = ifelse(file_version1$logtarg<0, 0, file_version1$logtarg)
# colnames(file_version1) = c('id','yhat')
# write.csv(file_version1, file=paste('./submission/model_',format(Sys.time(), "%b%d_%H%M%S"),'.csv',sep=''),row.names = FALSE)


########## Model Building Version 2
##### Description: Fit a classication first and then a regression model. Predict final result combing both model.

##### Classification
train_cla = train
train_cla$logtarg_bol = ifelse(train$logtarg!=0, 1, 0)
train_cla = train_cla %>% dplyr::select(-logtarg)

fit_classification = glm(logtarg_bol~.-id-total_duration, family=binomial, data=train_cla)
summary(fit_classification)

##### > custom
cla_feature = c('logtarg_bol','days_recent_purchase','days_first_purchase', 'order_count','coe_va')#, 'slope'
fit_classification_custom = glm(logtarg_bol~., family=binomial, data=train_cla[cla_feature])
summary(fit_classification_custom)


##### > Stepwise 
fit_stepback_cla <- stepAIC(fit_classification,direction = c("backward"))
summary(fit_stepback_cla) # Adjusted R-squared:  0.01663

##### Model selection
predict_fit = predict(fit_stepback_cla, newdata=train_cla, type="response")
real_response = train_cla$logtarg_bol

optimal_p = get_optimal_p(real_response, predict_fit, seq(0.03,0.20,0.005))
calculate_metrics(train_cla,real_response, predict_fit, optimal_p)
# Stepwise backward gives us a slightly better AUC of 0.677 instead of 0.676 for fit_classification that is without the feature slope.
# Therefore we choose fit_stepback_cla as our final model

pred_bol = rep(0, length(real_response))
pred_bol[predict_fit > optimal_p]=1

##### Regression
# use 'fit_stepback' in the previous version


##### Make prediction and Generating submitting file
file_version2 = combined_prediction(fit_stepback_cla, fit_stepback, test, optimal_p)
write.csv(file_version2, file=paste('./submission/model_',format(Sys.time(), "%b%d_%H%M%S"),'.csv',sep=''),row.names = FALSE)



##### Other note: To make sure the MSE in training goes down in after combining the result of classification and regression
# #calculate mean square for train data
# cla_predict = predict(fit_stepback_cla, newdata=train_cla, type="response")
# train_cla$predict_prob = cla_predict
# 
# train_reg_from_cla = train_cla %>% filter(predict_prob>0.06)
# reg_predict = predict(fit_multiple, newdata=train_reg_from_cla)
# train_reg_from_cla$reg_predict= reg_predict
# 
# train_final = merge(train_cla %>% dplyr::select(id), train_reg_from_cla %>% dplyr::select(id, reg_predict),by='id',all.x= TRUE)
# train_final[is.na(train_final)]=0
# train_final$prob = cla_predict
# train_final = train_final %>% mutate(final_predict= prob*reg_predict)
# 
# reg_predict = predict(fit_multiple, newdata=train)
# reg_predict = ifelse(reg_predict<0, 0, reg_predict)
# cla_predict = predict(fit_stepback_cla, newdata=train_cla, type="response")
# 
# real_response = train$logtarg
# 
# get_mse(train_final$final_predict, real_response)


