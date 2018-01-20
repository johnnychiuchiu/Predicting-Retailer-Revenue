library(dplyr)
library(ggplot2)
library(car)
library(MASS)
library(glmnet)
library(pROC)
if (!require(DAAG)) {
  install.packages('DAAG')
  library(DAAG)} else {
  library(DAAG)
}

if (!require(boot)) {
  install.packages('boot')
  library(boot)} else {
  library(boot)
}
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
train[,!names(train) %in% c("id","logtarg")] = scale(train[,!names(train) %in% c("id","logtarg")])
test[,!names(test) %in% c("id","logtarg")] = scale(test[,!names(test) %in% c("id","logtarg")])
system("say Just finished!")
########## Model Building Version 1
##### Description: Fit a simple regression model for benchmark
train_mult <- train[train$logtarg != 0,]

fit_multiple <- lm(logtarg~.-id + avg_qty:ordperyr:avg_ord_value + avg_qty:coe_va + total_duration:total_money, data = train_mult)
summary(fit_multiple)
vif(fit_multiple)

#cv.lm(data=train, m=10, form.lm=fit_multiple) # ms 0.375 

##### > Linear Regression with custom chosen variables
# chosen_feature = c('days_first_purchase', 'order_count', 'avg_qty',
#                     'slope' ,'coe_va', 'logtarg')#'avg_qty', 'days_recent_purchase', 
# fit_custom = lm(logtarg~., data = train[chosen_feature]) 
# summary(fit_custom)
# 
# cv.lm(data=train[chosen_feature], m=10, form.lm=fit_custom) # ms 0.375

##### > Stepwise Linear Regression
# **Backward**
fit_stepback <- stepAIC(fit_multiple,direction = c("backward"))
summary(fit_stepback) # Adjusted R-squared:  0.01663

cv.lm(data=train_mult, m=10, form.lm=fit_stepback) # ms ms 0.374

# # **Forward**
# fit_zero <- lm(y ~ 1, data = train)
# fit_stepforw <- stepAIC(fit_zero,direction = c("forward"), scope=list(upper=fit_multiple,lower=fit_zero))
# summary(fit_stepforw) # Adjusted R-squared:  0.007776

##### > Lasso Linear Regression
# y=train$logtarg
# x=model.matrix(logtarg~.-id-total_duration,train)
# param_lasso = my_cv_glmnet(y,x,1)$small.lambda.betas
# param_lasso = param_lasso[param_lasso!=0]
# lasso_feature = c('logtarg','days_recent_purchase', 'days_first_purchase', 'order_count', 'avg_qty',
#                 '6','8','12','20','30','38','50','coe_va')
# fit_lasso = lm(logtarg~., data = train[lasso_feature])
# summary(fit_lasso) #Adjusted R-squared:  0.01626
# 
# cv.lm(data=train[lasso_feature], m=10, form.lm=fit_lasso) 

##### Model selection
# choose the model according to Adjusted R-squared, Feature Significance, cv mean square error. 
# Multiple linear, custom and stepback give us similar result in Adjusted R^2 and cv ms.
# I'll choose fit_multiple as our final model now, since it somehow give us the best test error in my first try.


##### Make prediction
predict_reg = predict(fit_stepback, newdata=test)




# ########## Model Building Version 2
# ##### Description: Fit a classication first and then a regression model. Predict final result combing both model. 
# 


##### Logistic Regression
train_cla = train
train_cla$logtarg_bol = ifelse(train$logtarg!=0, 1, 0)
train_cla = train_cla %>% dplyr::select(-logtarg)

fit_classification = glm(logtarg_bol~.-id + avg_qty:order_count + avg_qty:coe_va + total_duration:ordperyr:avg_ord_value,
                         family=binomial, data=train_cla)
summary(fit_classification)

##### > custom
# cla_feature = c('logtarg_bol','days_recent_purchase','days_first_purchase', 'order_count','coe_va')#, 'slope'
# fit_classification_custom = glm(logtarg_bol~., family=binomial, data=train_cla[cla_feature])
# summary(fit_classification_custom)


##### > Stepwise 
fit_stepback_cla <- stepAIC(fit_classification,direction = c("backward"))
summary(fit_stepback_cla) 

##### Model selection
predict_fit = predict(fit_stepback_cla, newdata=test, type="response")
real_response = train_cla$logtarg_bol

#optimal_p = get_optimal_p(real_response, predict_fit, seq(0.03,0.20,0.005))
#calculate_metrics(train_cla,real_response, predict_fit, optimal_p)


##### Generating submitting file
test$logtarg = predict_fit * predict_reg
file_version1 = test %>% dplyr::select(id, logtarg)
file_version1$logtarg = ifelse(file_version1$logtarg<0, 0, file_version1$logtarg)
colnames(file_version1) = c('id','yhat')
write.csv(file_version1, file=paste('./submission/model_',format(Sys.time(), "%b%d_%H%M%S"),'.csv',sep=''),row.names = FALSE)



