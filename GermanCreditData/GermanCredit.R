library(rpart) # rpart for classification tree using CART
library(tree)
library(partykit) # ctree for 
library(rattle)
library(rpart.plot)

gcd_master <- read.csv("gcd.csv")

install.packages("XLConnect")
library(XLConnect)
require(XLConnect)
View(credit)

## a) Data Exploration

summaryStats <- as.data.frame(summary(credit[,c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_DEPENDENTS")]
))

# Categorical Variables: 7
# Numerical variables: 6
# Binary variables: 19

install.packages("plyr")
library(plyr)

# Frequency
freq_chk_acct = as.data.frame(xtabs(~ RESPONSE + CHK_ACCT, credit))

# Counting NAs in dataset
na_count <-sapply(credit, function(y) sum(length(which(is.na(y)))))
# There are no NAs in the dataset

to_categorical = c(1,2,4:10,12,13,15:22,24:26,28,30:32)
for(i in to_categorical) gcd_master[,i]=as.factor(gcd_master[,i])
gcd_use <- gcd_master[,2:32]

#################################################################################

# Tree using Gini parameter for splitting
training_tree_rpart_gini <- rpart(RESPONSE ~ .,data=gcd_use, method="class",control = rpart.control(minsplit = 10),parms = list(split = "gini"))
summary(training_tree_rpart_gini)
prp(training_tree_rpart_gini)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = gcd_use, type="class"), gcd_use$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_gini <- prune(training_tree_rpart_gini, cp=training_tree_rpart_gini$cptable[which.min(training_tree_rpart_gini$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_gini)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = gcd_use, type="class"), gcd_use$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using Entropy parameter for splitting
training_tree_rpart_info <- rpart(RESPONSE ~ .,data=gcd_use, method="class",control = rpart.control(minsplit = 10),parms = list(split = "information"))
summary(training_tree_rpart_info)
prp(training_tree_rpart_info)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_info, newdata = gcd_use, type="class"), gcd_use$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_info <- prune(training_tree_rpart_info, cp=training_tree_rpart_info$cptable[which.min(training_tree_rpart_info$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_info)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_info, newdata = gcd_use, type="class"), gcd_use$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using ctree from party package
training_tree_ctree <- ctree(RESPONSE ~.,data = gcd_use)
summary(training_tree_ctree)
plot(training_tree_ctree)

# accuracy
# training
prediction.matrix <- table(predict(training_tree_ctree,newdata = gcd_use), gcd_use$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#################################################################################

#### Using a split of 70:30 for training and test ####
partition_rows <- sample(1:nrow(gcd_use),700,replace=FALSE)
training_data <- gcd_use[partition_rows,]
testing_data <- gcd_use[-partition_rows,]

# Counting the number of 1s and 0s in training and test
counts_1s_training = sum(training_data$RESPONSE == 1, na.rm = TRUE)/nrow(training_data)
counts_1s_testing = sum(testing_data$RESPONSE == 1, na.rm = TRUE)/nrow(testing_data)
counts_1s_training 
counts_1s_testing

# Tree using Gini parameter for splitting
training_tree_rpart_gini <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "gini"))
#summary(training_tree_rpart_gini)
prp(training_tree_rpart_gini)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_gini <- prune(training_tree_rpart_gini, cp=training_tree_rpart_gini$cptable[which.min(training_tree_rpart_gini$cptable[,"xerror"]),"CP"])
summary(pruned_training_tree_rpart_gini)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using Entropy parameter for splitting
training_tree_rpart_info <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "information"))
summary(training_tree_rpart_info)
prp(training_tree_rpart_info)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_info, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_info <- prune(training_tree_rpart_info, cp=training_tree_rpart_info$cptable[which.min(training_tree_rpart_info$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_info)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy


#################################################################################

#### Using a split of 80:20 for training and test ####
partition_rows <- sample(1:nrow(gcd_use),800,replace=FALSE)
training_data <- gcd_use[partition_rows,]
testing_data <- gcd_use[-partition_rows,]

# Counting the number of 1s and 0s in training and test
counts_1s_training = sum(training_data$RESPONSE == 1, na.rm = TRUE)/nrow(training_data)
counts_1s_testing = sum(testing_data$RESPONSE == 1, na.rm = TRUE)/nrow(testing_data)
counts_1s_training 
counts_1s_testing

# Tree using Gini parameter for splitting
training_tree_rpart_gini <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "gini"))
#summary(training_tree_rpart_gini)
prp(training_tree_rpart_gini)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_gini <- prune(training_tree_rpart_gini, cp=training_tree_rpart_gini$cptable[which.min(training_tree_rpart_gini$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_gini)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using Entropy parameter for splitting
training_tree_rpart_info <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "information"))
#summary(training_tree_rpart_info)
prp(training_tree_rpart_info)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_info, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_info <- prune(training_tree_rpart_info, cp=training_tree_rpart_info$cptable[which.min(training_tree_rpart_info$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_info)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#################################################################################

#### Using a split of 90:10 for training and test #### 
partition_rows <- sample(1:nrow(gcd_use),900,replace=FALSE)
training_data <- gcd_use[partition_rows,]
testing_data <- gcd_use[-partition_rows,]

# Counting the number of 1s and 0s in training and test
counts_1s_training = sum(training_data$RESPONSE == 1, na.rm = TRUE)/nrow(training_data)
counts_1s_testing = sum(testing_data$RESPONSE == 1, na.rm = TRUE)/nrow(testing_data)
counts_1s_training 
counts_1s_testing

# Tree using Gini parameter for splitting
training_tree_rpart_gini <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "gini"))
#summary(training_tree_rpart_gini)
prp(training_tree_rpart_gini)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_gini <- prune(training_tree_rpart_gini, cp=training_tree_rpart_gini$cptable[which.min(training_tree_rpart_gini$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_gini)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using Entropy parameter for splitting
training_tree_rpart_info <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "information"))
#summary(training_tree_rpart_info)
prp(training_tree_rpart_info)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_info, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_info <- prune(training_tree_rpart_info, cp=training_tree_rpart_info$cptable[which.min(training_tree_rpart_info$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_info)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#################################################################################

#### Using a split of 65:35 for training and test #### 
partition_rows <- sample(1:nrow(gcd_use),650,replace=FALSE)
training_data <- gcd_use[partition_rows,]
testing_data <- gcd_use[-partition_rows,]

# Counting the number of 1s and 0s in training and test
counts_1s_training = sum(training_data$RESPONSE == 1, na.rm = TRUE)/nrow(training_data)
counts_1s_testing = sum(testing_data$RESPONSE == 1, na.rm = TRUE)/nrow(testing_data)
counts_1s_training 
counts_1s_testing

# Tree using Gini parameter for splitting
training_tree_rpart_gini <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "gini"))
#summary(training_tree_rpart_gini)
prp(training_tree_rpart_gini)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_gini <- prune(training_tree_rpart_gini, cp=training_tree_rpart_gini$cptable[which.min(training_tree_rpart_gini$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_gini)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using Entropy parameter for splitting
training_tree_rpart_info <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "information"))
#summary(training_tree_rpart_info)
prp(training_tree_rpart_info)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_info, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_info <- prune(training_tree_rpart_info, cp=training_tree_rpart_info$cptable[which.min(training_tree_rpart_info$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_info)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#################################################################################

#################################################################################

#### Using a split of 50:50 for training and test #### 
partition_rows <- sample(1:nrow(gcd_use),500,replace=FALSE)
training_data <- gcd_use[partition_rows,]
testing_data <- gcd_use[-partition_rows,]

# Counting the number of 1s and 0s in training and test
counts_1s_training = sum(training_data$RESPONSE == 1, na.rm = TRUE)/nrow(training_data)
counts_1s_testing = sum(testing_data$RESPONSE == 1, na.rm = TRUE)/nrow(testing_data)
counts_1s_training 
counts_1s_testing

# Tree using Gini parameter for splitting
training_tree_rpart_gini <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "gini"))
#summary(training_tree_rpart_gini)
prp(training_tree_rpart_gini)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_gini <- prune(training_tree_rpart_gini, cp=training_tree_rpart_gini$cptable[which.min(training_tree_rpart_gini$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_gini)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# Tree using Entropy parameter for splitting
training_tree_rpart_info <- rpart(RESPONSE ~ .,data=training_data, method="class",control = rpart.control(minsplit = 10),parms = list(split = "information"))
#summary(training_tree_rpart_info)
prp(training_tree_rpart_info)

# accuracy
# training
prediction.matrix = table(predict(training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

# testing
prediction.matrix = table(predict(training_tree_rpart_info, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#pruning
pruned_training_tree_rpart_info <- prune(training_tree_rpart_info, cp=training_tree_rpart_info$cptable[which.min(training_tree_rpart_info$cptable[,"xerror"]),"CP"])
prp(pruned_training_tree_rpart_info)
# accuracy
# training
prediction.matrix = table(predict(pruned_training_tree_rpart_info, newdata = training_data, type="class"), training_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy
# testing
prediction.matrix = table(predict(pruned_training_tree_rpart_gini, newdata = testing_data, type="class"), testing_data$RESPONSE)
prediction.matrix
accuracy<-sum(diag(prediction.matrix))/sum(prediction.matrix)
accuracy

#################################################################################


GCD_MisClassificaton <- rpart(RESPONSE ~ ., data = training_data, method = "class", control = rpart.control(minsplit = 10), parms = list(split="gini", loss = matrix(c(0,100,500,0), byrow = TRUE, nrow = 2))) 
summary(GCD_MisClassificaton)
prp(GCD_MisClassificaton)

#Prediction and Accuracy of Training Data

pred_rpart_cost_train <- predict(GCD_MisClassificaton, newdata = training_data, type="class")
pred_rpart_GCDCostTree_train <- table(pred_rpart_cost_train, training_data$RESPONSE, dnn = c("predicted","actual"))
pred_rpart_GCDCostTree_train
accu_rpart_GCDCostTree_train <- sum(diag(pred_rpart_GCDCostTree_train))/sum(pred_rpart_GCDCostTree_train)
accu_rpart_GCDCostTree_train

#Prediction and Accuracy of Test Data

pred_rpart_cost_test <- predict(GCD_MisClassificaton, newdata = testing_data, type="class")
pred_rpart_GCDCostTree_test <- table(pred_rpart_cost_test, testing_data$RESPONSE, dnn = c("predicted","actual"))
pred_rpart_GCDCostTree_test
accu_rpart_GCDCostTree_test <- sum(diag(pred_rpart_GCDCostTree_test))/sum(pred_rpart_GCDCostTree_test)
accu_rpart_GCDCostTree_test