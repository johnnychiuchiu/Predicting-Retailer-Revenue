library(dplyr)
library(ggplot2)
library(car)
library(MASS)
library(glmnet)
options(scipen=999) # remove scientific notation in printing


########## Working directory and source function
pwd = getwd()
source(paste(pwd,'/src/function.R',sep=''))


########## Read Data
booktrain_path = paste(getwd(),'/data/booktrain.csv',sep='')
booktrain = read.csv(booktrain_file)

orders_path = paste(getwd(),'/data/orders.csv',sep='')
orders = read.csv(orders_file)


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


########## Model Building Version 1
##### Description: Fit a simple regression model for benchmark
fit_multiple = lm(logtarg~.-id-total_duration, data = train)
summary(fit_multiple)
vif(fit_multiple)

##### > Stepwise Linear Regression
# **Backward**
fit_stepback <- stepAIC(fit_multiple,direction = c("backward"))
summary(fit_stepback) # Adjusted R-squared:  0.01663

# **Forward**
fit_zero <- lm(y ~ 1, data = train)
fit_stepforw <- stepAIC(fit_zero,direction = c("forward"), scope=list(upper=fit_multiple,lower=fit_zero))
summary(fit_stepforw) # Adjusted R-squared:  0.007776

##### > Lasso Linear Regression
y=train$logtarg
x=model.matrix(logtarg~.-id-total_duration,train)
param_lasso = my_cv_glmnet(y,x,1)$small.lambda.betas
param_lasso = param_lasso[param_lasso!=0]
lasso_feature = c('logtarg','days_recent_purchase', 'days_first_purchase', 'order_count', 'avg_qty',
                '6','8','12','20','30','38','50','coe_va')
fit_lasso = lm(logtarg~., data = train[lasso_feature])
summary(fit_lasso) #Adjusted R-squared:  0.01626

##### Model selection
# choose the model according to Adjusted R-squared. The highest is the model from Stepwise Backward


##### Make prediction
predict_version1 = predict(fit_stepback, newdata=test)

##### Generating submitting file
test$logtarg = predict_version1
file_version1 = test %>% dplyr::select(id, logtarg)
colnames(file_version1) = c('id','yhat')
write.csv(file_version1, file=paste('./submission/model_',format(Sys.time(), "%b%d_%H%M%S"),'.csv',sep=''),row.names = FALSE)



########## Model Building Version 2
##### Description: Fit a classication first and then a regression model. Predict final result combing both model. 

##### Classification

##### Regression

##### > Multiple Linear Regression
##### > Stepwise Linear Regression
##### > Lasso Linear Regression
##### > Ridge Linear Regression

##### Model Selection


##### Generating submitting file




