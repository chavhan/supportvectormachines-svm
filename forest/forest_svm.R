############ SVM on forestfires dataset ##############
forest_data <- forestfires
head(forest_data)
forest_data <- forest_data[c(-1,-2)]
forest_data$size_category <- as.factor(forest_data$size_category)
partition <- createDataPartition(forest_data$size_category,p=.75,list = F)  ## partitioning data 
train_data <- forest_data[partition,]
test_data <- forest_data[-partition,]
head(train_data)
library(kernlab)
vanilla_model <- ksvm(train_data$size_category~.,data = train_data, kernel = 'rbfdot')
vanilla_model
#################### Checking accuracy on train data #################
vanilla_pred <- predict(vanilla_model,train_data)
vanilla_pred
vanilla_tab <- table(vanilla_pred,train_data$size_category)
vanilla_tab
sum(diag(vanilla_tab))/sum(vanilla_tab)   ## accuracy vanilla-0.8868895,rbfdot- 0.781491

#################### Checking accuracy on test data ##################
vanilla_pred_test <- predict(vanilla_model,test_data)
vanilla_pred_test
vanilla_tab_test <- table(vanilla_pred_test,test_data$size_category)
vanilla_tab_test
sum(diag(vanilla_tab_test))/sum(vanilla_tab_test)   ## accuracy vanilla-0.9140625,rbfdot-0.765625

