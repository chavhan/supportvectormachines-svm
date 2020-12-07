########################## SVM on salary dataset ###################

## converting categorical values into numeric 
## For train dataset 
s_numeric <- s_train
head(s_numeric)
View(s_numeric)
s_numeric$workclassno <- as.numeric(as.factor(s_numeric$workclass))
s_numeric$maritalstatusno <- as.numeric(as.factor(s_numeric$maritalstatus))
s_numeric$occupationno <- as.numeric(as.factor(s_numeric$occupation))
s_numeric$relationshipno <- as.numeric(as.factor(s_numeric$relationship))
s_numeric$raceno  <- as.numeric(as.factor(s_numeric$race))
s_numeric$nativeno <- as.numeric(as.factor(s_numeric$native))
s_numeric$sexno <- as.numeric(as.factor(s_numeric$sex))
s_numeric$salaryno <- as.numeric(as.factor(s_numeric$Salary))

s_filter <- s_numeric[,c(-2,-3,-5,-6,-7,-8,-9,-13,-14)]
head(s_filter)


convert_value <- function(x)
{
  if(x == 1)
    return(0)
  if(x == 2)
    return(1)
}
View(s_train_filter[1:10, ])
s_filter$salaryno <- as.factor(sapply(s_filter$salaryno,convert_value))

library(caret)
partition <- createDataPartition(s_filter$salaryno,p=.75,list = F)
s_train <- s_filter[partition,]
s_test <- s_filter[-partition,]
dim(s_filter)

library(kernlab)
vanilla_model <- ksvm(s_train$salaryno~.,data = s_train, kernel = 'rbfdot')

#################### Checking accuracy on train data #################
vanilla_pred <- predict(vanilla_model,s_train)
vanilla_pred
vanilla_tab <- table(vanilla_pred,s_train$salaryno)
vanilla_tab
sum(diag(vanilla_tab))/sum(vanilla_tab)   ## accuracy vanilla-0.8067286,rbfdot- 0.8556884

#################### Checking accuracy on test data ##################
vanilla_pred_test <- predict(vanilla_model,s_test)
vanilla_pred_test
vanilla_tab_test <- table(vanilla_pred_test,s_test$salaryno)
vanilla_tab_test
sum(diag(vanilla_tab_test))/sum(vanilla_tab_test)   ## accuracy vanilla-0.8045153,rbfdot-0.8398406