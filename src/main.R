library(dplyr)
library(ggplot2)
library(car)
options(scipen=999) # remove scientific notation in printing


########## Working directory and source function
pwd = getwd()
source(paste(pwd,'/src/function.R',sep=''))


########## Read Data
booktrain_file = paste(getwd(),'/data/booktrain.csv',sep='')
booktrain = read.csv(booktrain_file)

orders_file = paste(getwd(),'/data/orders.csv',sep='')
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

##### Make prediction
predict_version1 = predict(fit_multiple, newdata=test)

##### Generating submitting file
test$logtarg = predict_version1
file_version1 = test %>% select(id, logtarg)
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




