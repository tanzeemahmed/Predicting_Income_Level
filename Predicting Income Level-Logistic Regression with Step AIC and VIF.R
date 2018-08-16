rm(list = ls(all=TRUE))

# Importing necessary Libraries


library(DMwR)

library(caret)

library(dplyr)

#Read in the train dataset

income_data <- read.csv("train_data.csv",header=TRUE)


#Structure and Summary of the Dataset

str(income_data)

summary(income_data)

#Remove the index

income_data$index <- NULL

#Separate the target variable from the data

target_variable <- income_data$target

income_data$target <- NULL

#Head and tail of the data

head(income_data,10)

tail(income_data)

#Convert the variables into appropriate types

income_data$loan_taken <- as.factor(as.character(income_data$loan_taken))

# Check for Missing Values

colSums(is.na(income_data))

# Since the no. of misisng values in Tax_Paid is high, we can group the values in 3 levels - Low tax Paid for tax less than the mean value, High Tax Paid for tax more than the mean value and No Tax Paid for the NA's.

# Imputing Tax Paid NA's

income_data$tax_paid <- ifelse(income_data$tax_paid <6200 ,"Low Tax Paid",ifelse(income_data$tax_paid>6200,"High tax paid","No tax paid"))

income_data$tax_paid[is.na(income_data$tax_paid)] <- "No Tax Paid"

income_data$tax_paid <- as.factor(income_data$tax_paid)

str(income_data)

#We can impute the other Missing values by using KNN Impute of Dmwr library (similar rows)

num_attr <- c("age","financial_weight","gain","loss","working_hours")

num_attr_data <- income_data[,num_attr]
cat_attr_data <- income_data[,!names(income_data) %in% num_attr]

str(cat_attr_data)


colSums(is.na(num_attr_data))

num_attr_data_imputed <- knnImputation(data = num_attr_data,k = 10,meth = "weighAvg")

colSums(is.na(num_attr_data_imputed))

cat_attr_data_imputed <- knnImputation(data = cat_attr_data,k=10,meth = "median")

colSums(is.na(cat_attr_data_imputed))

sum(is.na(income_data))

#Merge the num_attr and cat_attr and also the target variable

income_data_imputed <- cbind(num_attr_data_imputed,cat_attr_data_imputed,target_variable)

class(income_data_imputed)

str(income_data_imputed)

####Split the data into Train and Validation

set.seed(786)

train_rows <- createDataPartition(target_variable,p=0.7,list=F)

train_data <- income_data_imputed[train_rows,]

test_data <- income_data_imputed[-train_rows,]



#Read test data and convert the variables appropriately

test_data <- read.csv("test_data.csv",header = TRUE)

test_data$loan_taken <- as.factor(as.character(test_data$loan_taken))

test_data$tax_paid <- ifelse(test_data$tax_paid <6200 ,"Low Tax Paid",ifelse(test_data$tax_paid>6200,"High tax paid","No tax paid"))

test_data$tax_paid[is.na(test_data$tax_paid)] <- "No Tax Paid"

test_data$tax_paid <- as.factor(test_data$tax_paid)

str(test_data)

# Check for Missing Values in test _data

colSums(is.na(test_data))

#Remove index and reassign it later

index <- test_data$index

test_data$index <- NULL

#We can impute the other Missing values by using KNN Impute of Dmwr library (similar rows)

num_attr_data_test <- test_data[,num_attr]
cat_attr_data_test <- test_data[,!names(test_data) %in% num_attr]

str(num_attr_data_test)
str(cat_attr_data_test)


colSums(is.na(num_attr_data_test))

num_attr_data_test_imputed <- knnImputation(data = num_attr_data_test,k = 10,meth = "weighAvg")

colSums(is.na(num_attr_data_test))

cat_attr_data__test_imputed <- knnImputation(data = cat_attr_data_test,k=10,meth = "median")

colSums(is.na(cat_attr_data__test_imputed))

str(num_attr_data_test_imputed)
str(cat_attr_data__test_imputed)


sum(is.na(income_data))

#Merge the num_attr and cat_attr for the test data

test_data_imputed <- cbind(num_attr_data_test_imputed,cat_attr_data__test_imputed)

class(income_data_imputed)

str(test_data_imputed)


#Build a model


log_reg <- glm(target_variable~., data = income_data_imputed, family = binomial)

summary(log_reg)


#Predict the values on Train data

prob_train <- predict(log_reg,type="response")
head(prob_train)

#Create a Prediction object using ROCR

library(ROCR)

pred <- prediction(prob_train,income_data_imputed$target)

#Extract performance measure 

perf <- performance(pred,measure = "tpr",x.measure = "fpr")

#Plot ROC Curve

plot(perf,col=rainbow(10),colorize=T,print.cutoffs.at=seq(0,1,0.05))

#Plot AUC Curve

perf_auc <- performance(pred,measure = "auc")

perf_auc

perf_auc@y.values[[1]]

#Choose cutoff at 0.1

plot(perf,col=rainbow(10), colorize = T,print.cutoffs.at=0.1)

prob_test <- predict(log_reg,test_data_imputed,type = "response")

preds_test <-ifelse(prob_test>0.6,"1","0")

length(preds_test)

#Merge test data with predicted values



final_data <-cbind(index,test_data_imputed,preds_test)

names(final_data)[18] = "target"

submission <- final_data[,names(final_data) %in% c("index","target") ]

#Write to CSV file 

library(xlsx)

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/Cute/Submission-0.6-non-scaled.csv")



#Build an AIC model 

library(MASS)

model_aic <- stepAIC(log_reg, direction = "both")

summary(model_aic)

#Predict the values on Train data

prob_train <- predict(model_aic,type="response")
head(prob_train)

#Create a Prediction object using ROCR

library(ROCR)

pred <- prediction(prob_train,income_data_imputed$target)

#Extract performance measure 

perf <- performance(pred,measure = "tpr",x.measure = "fpr")

#Plot ROC Curve

plot(perf,col=rainbow(10),colorize=T,print.cutoffs.at=seq(0,1,0.05))

#Plot AUC Curve

perf_auc <- performance(pred,measure = "auc")

perf_auc

perf_auc@y.values[[1]]

#Choose cutoff at 0.1

plot(perf,col=rainbow(10), colorize = T,print.cutoffs.at=0.1)

prob_test <- predict(log_reg,test_data_imputed,type = "response")

preds_test <-ifelse(prob_test>0.6,"1","0")

length(preds_test)

#Merge test data with predicted values



final_data <-cbind(index,test_data_imputed,preds_test)

names(final_data)[18] = "target"

submission <- final_data[,names(final_data) %in% c("index","target") ]

#Write to CSV file 

library(xlsx)

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/Cute/Submission-stepAIC.csv")


#######Apply VIF


log_reg <- glm(target_variable~.-tax_paid-loan_taken, data = income_data_imputed, family = binomial)

summary(log_reg)

#Predict the values on Train data

prob_train <- predict(log_reg,type="response")

head(prob_train)

#Create a Prediction object using ROCR

library(ROCR)

pred <- prediction(prob_train,income_data_imputed$target)

#Extract performance measure 

perf <- performance(pred,measure = "tpr",x.measure = "fpr")

#Plot ROC Curve

plot(perf,col=rainbow(10),colorize=T,print.cutoffs.at=seq(0,1,0.05))

#Plot AUC Curve

perf_auc <- performance(pred,measure = "auc")

perf_auc

perf_auc@y.values[[1]]

#Choose cutoff at 0.1

plot(perf,col=rainbow(10), colorize = T,print.cutoffs.at=0.1)

prob_test <- predict(log_reg,test_data_imputed,type = "response")

preds_test <-ifelse(prob_test>0.6,"1","0")

length(preds_test)

#Merge test data with predicted values



final_data <-cbind(index,test_data_imputed,preds_test)

names(final_data)[18] = "target"

submission <- final_data[,names(final_data) %in% c("index","target") ]

#Write to CSV file 

library(xlsx)

write.csv(submission, "C:/Users/Yunus Saleem/Desktop/Insofe/Cute/Submission-removing_tax_loan.csv")


