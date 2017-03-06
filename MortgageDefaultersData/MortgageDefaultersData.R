# Installing the required packages
install.packages("ROSE")
install.packages("devtools")
install.packages("ROCR")
install.packages("CART")
library(ROCR)
library(devtools)
library(ROSE)
library(ggplot2)

#### Pre processing data ####

#Reading the file MortgageDeafultersData
MortgageData <- read.csv("MortgageDefaultersData.csv")

#Reading the file state
StateData <- read.csv("StateData.csv")

#Merging the 2 files by State
MortgageDefaulterData <- merge(MortgageData, StateData, by = "State", all.x=TRUE)
names(MortgageDefaulterData)[16:17] <- c("avg_income","pct_poverty")

#Removing Rows whose LoanValuetoAppraised column's value is not equal to #DIV/0!
MortgageDefaulterData <- subset(MortgageDefaulterData, MortgageDefaulterData$LoanValuetoAppraised != "#DIV/0!")

#Converting categorical columns to a factor
MortgageDefaulterData$UPB.Appraisal <- as.factor(MortgageDefaulterData$UPB.Appraisal)
MortgageDefaulterData$OUTCOME <- as.factor(MortgageDefaulterData$OUTCOME)
MortgageDefaulterData$State_Code <- as.factor(MortgageDefaulterData$State_Code)
MortgageDefaulterData$First_home <- as.factor(MortgageDefaulterData$First_home)
MortgageDefaulterData$Status <- as.factor(MortgageDefaulterData$Status)

#Removing State_Code column since it is a repetitive
MortgageDefaulterData$State_Code <- NULL

#Exploring the data
nrow(MortgageDefaulterData)
ncol(MortgageDefaulterData)
dim(MortgageDefaulterData)
summary(MortgageDefaulterData)
str(MortgageDefaulterData)

#Checking for Missing Data
sum(is.na(MortgageDefaulterData))

#Checking for Duplicates
which(duplicated(MortgageDefaulterData) | duplicated(MortgageDefaulterData, fromLast = TRUE))

#Class Outcome
table(MortgageDefaulterData$OUTCOME)

#Frequency Table (Class Distrbution)
prop.table(table(MortgageDefaulterData$OUTCOME))

#Converting avg_income column into numeric
MortgageDefaulterData$avg_income <- as.character(MortgageDefaulterData$avg_income)
MortgageDefaulterData$avg_income <- as.numeric(MortgageDefaulterData$avg_income)

#Finding Outliers and Replacing them with NA 
MortgageDefaulterData_NoOutlier <- MortgageDefaulterData

MortgageDefaulterData_NoOutlier$Credit_score <- ifelse(MortgageDefaulterData_NoOutlier$Credit_score >= 900,NA,MortgageDefaulterData_NoOutlier$Credit_score)

Outliers <- boxplot(MortgageDefaulterData_NoOutlier$Tot_mthly_incm)$out
MortgageDefaulterData_NoOutlier$Tot_mthly_incm <- ifelse(MortgageDefaulterData_NoOutlier$Tot_mthly_incm %in% Outliers, NA, MortgageDefaulterData_NoOutlier$Tot_mthly_incm)

MortgageDefaulterData_NoOutlier$LoanValuetoAppraised <- as.numeric(MortgageDefaulterData_NoOutlier$LoanValuetoAppraised)*1

#checking for NAs
sum(is.na(MortgageDefaulterData_NoOutlier))

#Removing Records having NA's
MortgageDefaulterData_Final <- na.omit(MortgageDefaulterData_NoOutlier)

#checking for NAs after treatment (must return 0)
sum(is.na(MortgageDefaulterData_Final))

#Backup of data
Mortgage_Backup <- MortgageDefaulterData
Mortgage_Backup_No_Outlier <- MortgageDefaulterData_Final

#### Normalization ####

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

MortgageDefaulterData_Final_norm <- MortgageDefaulterData_Final
MortgageDefaulterData_Final_norm$Bo_Age <- normalize(MortgageDefaulterData_Final$Bo_Age)
MortgageDefaulterData_Final_norm$Ln_Orig <- normalize(MortgageDefaulterData_Final$Ln_Orig)
MortgageDefaulterData_Final_norm$Orig_LTV_Ratio_Pct <- normalize(MortgageDefaulterData_Final$Orig_LTV_Ratio_Pct)
MortgageDefaulterData_Final_norm$Credit_score <- normalize(MortgageDefaulterData_Final$Credit_score)
MortgageDefaulterData_Final_norm$Tot_mthly_debt_exp <- normalize(MortgageDefaulterData_Final$Tot_mthly_debt_exp)
MortgageDefaulterData_Final_norm$Tot_mthly_incm <- normalize(MortgageDefaulterData_Final$Tot_mthly_incm)
MortgageDefaulterData_Final_norm$orig_apprd_val_amt <- normalize(MortgageDefaulterData_Final$orig_apprd_val_amt)
MortgageDefaulterData_Final_norm$pur_prc_amt <- normalize(MortgageDefaulterData_Final$pur_prc_amt)
MortgageDefaulterData_Final_norm$DTI.Ratio <- normalize(MortgageDefaulterData_Final$DTI.Ratio)
MortgageDefaulterData_Final_norm$LoanValuetoAppraised <- normalize(MortgageDefaulterData_Final$LoanValuetoAppraised)
MortgageDefaulterData_Final_norm$pct_poverty <- normalize(MortgageDefaulterData_Final$pct_poverty)
MortgageDefaulterData_Final_norm$avg_income <- normalize(MortgageDefaulterData_Final$avg_income)

table(MortgageDefaulterData_Final_norm$OUTCOME)

#Converting Character Variables to Factors 
MortgageDefaulterData_Final_norm$OUTCOME <- as.factor(MortgageDefaulterData_Final_norm$OUTCOME)
MortgageDefaulterData_Final_norm$First_home <- as.factor(MortgageDefaulterData_Final_norm$First_home)
MortgageDefaulterData_Final_norm$Status <- as.factor(MortgageDefaulterData_Final_norm$Status)
str(MortgageDefaulterData_Final_norm)

# ----------- Data Preparation ------------------------------------

MortgagedataFinal <- MortgageDefaulterData_Final_norm 

## Histograms of Important Variables from the original dataset
str(MortgagedataFinal)

hist(table(MortgagedataFinal$Status), freq=TRUE, main="Histogram for Status",     xlab= levels(MortgagedataFinal$Status),     border="Red",     col="cadetblue3")
hist(MortgagedataFinal$Credit_score, main="Histogram for Credit Score",     xlab="Credit Score",     border="Red",     col="cadetblue3")
hist(MortgagedataFinal$DTI.Ratio, main="Histogram for DTI Ratio",     xlab="DTI Ratio",     border="Red",     col="cadetblue3")
hist(MortgagedataFinal$pur_prc_amt, main="Histogram for Percentage of Purchased Amount",     xlab="Percentage of Purchased Amount",     border="Red",     col="cadetblue3")
hist(MortgagedataFinal$orig_apprd_val_amt, main="Histogram for Original Approved Value Amount",     xlab="Original Approved Value Amount",     border="Red",     col="cadetblue3")
hist(MortgagedataFinal$Tot_mthly_debt_exp, main="Histogram for Total Monthly Debt Expenses",     xlab="Total Monthly Debt Expenses",     border="Red",     col="cadetblue3")
hist(MortgagedataFinal$LoanValuetoAppraised, main="Histogram for Loan Value To Appraised",     xlab="Loan Value To Appraised",     border="Red",     col="cadetblue3")

#Removing Status from both the datasets as it is biased
MortgagedataFinal$Status <- NULL

#Removing Status from both the datasets as it is biased
MortgagedataFinal$State <- NULL

#Removing DTI.Ratio from both the datasets since it is a ratio of the columns Tot_mthly_debt_exp and Tot_mthly_incm
MortgagedataFinal$DTI.Ratio <- NULL

#### imbalanced sampling ####

#30% Default records
TrgA <- ovun.sample(OUTCOME ~ ., data = MortgagedataFinal, method = "under",seed= 2014,p = 0.3)$data
table(TrgA$OUTCOME)
prop.table(table(TrgA$OUTCOME))

#10% Default Records
TrgB <- ovun.sample(OUTCOME ~ ., data = MortgagedataFinal, method = "under",seed= 2014,p = 0.1)$data
table(TrgB$OUTCOME)
prop.table(table(TrgB$OUTCOME))

#Back up of TrgA and TrgB dataset
TrgA_BackUp <- TrgA
TrgB_BackUp <- TrgB

#### Splitting TrgA dataset into Training (75%) and Testing (25%) data ####

set.seed(2014) 
ind <- sample(2, nrow(TrgA), replace=TRUE, prob=c(0.75, 0.25)) 
TrgA_TrainingData <- TrgA[ind==1,] 
TrgA_TestData <- TrgA[ind==2,]

prop.table(table(TrgA_TrainingData$OUTCOME))
prop.table(table(TrgA_TestData$OUTCOME))

#### Splitting TrgB dataset into Training (75%) and Testing (25%) data ####

set.seed(2014) 
ind <- sample(2, nrow(TrgB), replace=TRUE, prob=c(0.75, 0.25)) 
TrgB_TrainingData <- TrgB[ind==1,] 
TrgB_TestData <- TrgB[ind==2,]

prop.table(table(TrgB_TrainingData$OUTCOME))
prop.table(table(TrgB_TestData$OUTCOME))

#### Splitting Original dataset into Training (75%) and Testing (25%) data ####

set.seed(2014) 
ind <- sample(2, nrow(MortgagedataFinal), replace=TRUE, prob=c(0.75, 0.25)) 
MortgagedataFinal_TrainingData <- MortgagedataFinal[ind==1,] 
MortgagedataFinal_TestData <- MortgagedataFinal[ind==2,]

prop.table(table(MortgagedataFinal_TrainingData$OUTCOME))
prop.table(table(MortgagedataFinal_TestData$OUTCOME))

#TrainingData <- MortgagedataFinal_TrainingData
TrainingData <- TrgA_TrainingData
#TrainingData <- TrgB_TrainingData

#TestData <- MortgagedataFinal_TestData
TestData <- TrgA_TestData
#TestData <- TrgB_TestData

#### Building a Decision Tree Classifier ####

library(rpart)
library(rpart.plot)

### Using information gain ###
MortgageTree_rpart_Info <- rpart(OUTCOME ~ ., data = TrainingData, method = "class", parms = list(split="information"))
plot(MortgageTree_rpart_Info, uniform = T, main="rpart decision tree with Entropy impurity function")
text(MortgageTree_rpart_Info, use.n = T, all = T, cex = 0.7, xpd = T)
printcp(MortgageTree_rpart_Info)
prp(MortgageTree_rpart_Info, main="rpart decision tree with Entropy impurity function")

#Prediction and accuracy of the Test Data
MortgageTree_Prediction_Info <- table(predict(MortgageTree_rpart_Info, newdata = TestData, type = "class"), TestData$OUTCOME)
MortgageTree_Prediction_Info
MortgageTree_Accuracy_Info <- sum(diag(MortgageTree_Prediction_Info))/sum(MortgageTree_Prediction_Info)
MortgageTree_Accuracy_Info

# pruning
opt <- which.min(MortgageTree_rpart_Info$cptable[, "xerror"]) #get lowest xerror for all trees
cp_min_xerror <- MortgageTree_rpart_Info$cptable[opt, "CP"] #CP corresponding to lowest xerror
MortgageTree_rpart_Pruned <- prune(MortgageTree_rpart_Info, cp = cp_min_xerror)
plot(MortgageTree_rpart_Pruned, main="Pruned rpart decision tree with Entropy impurity function")
printcp(MortgageTree_rpart_Pruned)

#Prediction and accuracy of the Test Data after Pruning
MortgageTree_Pruned_Prediction_Info <- table(predict(MortgageTree_rpart_Pruned, newdata = TestData, type = "class"), TestData$OUTCOME)
MortgageTree_Pruned_Prediction_Info
MortgageTree_Pruned_Accuracy_Info <- sum(diag(MortgageTree_Pruned_Prediction_Info))/sum(MortgageTree_Pruned_Prediction_Info)
MortgageTree_Pruned_Accuracy_Info

### Using Gini index ###
MortgageTree_rpart_Gini <- rpart(OUTCOME ~ ., data = TrainingData, method = "class", parms = list(split="gini"), control = rpart.control(xval=5,cp=0))
plot(MortgageTree_rpart_Gini, uniform = T, main="rpart decision tree with Entropy impurity function")
text(MortgageTree_rpart_Gini, use.n = T, all = T, cex = 0.7, xpd = T)
printcp(MortgageTree_rpart_Gini)
prp(MortgageTree_rpart_Gini, main="rpart decision tree with Entropy impurity function")

MortgageTree_rpart_Gini$variable.importance

#Prediction and accuracy of the Test Data
MortgageTree_Prediction_Gini_vector <- predict(MortgageTree_rpart_Gini, newdata = TestData, type = "class")
MortgageTree_Prediction_Gini <- table(MortgageTree_Prediction_Gini_vector, TestData$OUTCOME)
MortgageTree_Prediction_Gini
MortgageTree_Accuracy_Gini <- sum(diag(MortgageTree_Prediction_Gini))/sum(MortgageTree_Prediction_Gini)
MortgageTree_Accuracy_Gini

# Evaluation of model

#confusion matrix
library(caret)
confusionMatrix(MortgageTree_Prediction_Gini)

#ROC Curve
MortgageTree_Prediction_DF <- as.data.frame(MortgageTree_Prediction_Gini_vector)
MortgageTree_Prediction_DF$OUTCOME <- TestData$OUTCOME
MortgageTree_Prediction_DF$MortgageTree_Prediction_Gini <- as.numeric(MortgageTree_Prediction_DF$MortgageTree_Prediction_Gini)
MortgageTree_Prediction_DF$OUTCOME <- as.numeric(MortgageTree_Prediction_DF$OUTCOME)

MortgageTree_Prediction_ROCR <- ROCR::prediction(predictions = MortgageTree_Prediction_DF$MortgageTree_Prediction_Gini,labels = MortgageTree_Prediction_DF$OUTCOME)
MortgageTree_Performance_ROCR <- performance(MortgageTree_Prediction_ROCR,"tpr","fpr")
plot(MortgageTree_Performance_ROCR, lty = 3, lwd = 3, main="ROCR Curve")
MortgageTree_auc_obj <- performance(MortgageTree_Prediction_ROCR, "auc" ) 
MortgageTree_auc_val <- unlist(slot(MortgageTree_auc_obj,"y.values"))

MortgageTree_MINauc = min(round(MortgageTree_auc_val, digits = 2))
MortgageTree_MAXauc = max(round(MortgageTree_auc_val, digits = 2))
MortgageTree_MINauct = paste(c("min(AUC) = "), MortgageTree_MINauc, sep = "")
MortgageTree_MAXauct = paste(c("max(AUC) = "), MortgageTree_MAXauc, sep = "")
legend(0.7, 0.5, c(MortgageTree_MINauct, MortgageTree_MAXauct, "\n"), border = "white", cex = 0.7, box.col = "white")
abline(a= 0, b=1)

#Lift chart

MortgageTree_Performance_Lift = performance(MortgageTree_Prediction_ROCR,"lift","rpp")
plot(MortgageTree_Performance_ROCR,main="lift chart")

#### Building a Random Forest Classifier ####

library(randomForest)

MortgageTree_randomForest = randomForest(OUTCOME ~ ., data = TrainingData, ntree = 2000, mtry = 5)
print(MortgageTree_randomForest)
attributes(MortgageTree_randomForest)
plot(MortgageTree_randomForest)
importance(MortgageTree_randomForest)

MortgageTree_rF_Prediction_vector <- predict(MortgageTree_randomForest, newdata = TestData) 
MortgageTree_rF_Prediction <- table(MortgageTree_rF_Prediction_vector, TestData$OUTCOME)
MortgageTree_rF_Prediction
MortgageTree_rF_Accuracy <- sum(diag(MortgageTree_rF_Prediction))/sum(MortgageTree_rF_Prediction)
MortgageTree_rF_Accuracy

# Evaluation of model

#confusion matrix
library(caret)
confusionMatrix(MortgageTree_rF_Prediction)

#ROC Curve
MortgageTree_Prediction_DF <- as.data.frame(MortgageTree_rF_Prediction_vector)
MortgageTree_Prediction_DF$OUTCOME <- TestData$OUTCOME
MortgageTree_Prediction_DF$MortgageTree_rF_Prediction <- as.numeric(MortgageTree_Prediction_DF$MortgageTree_rF_Prediction)
MortgageTree_Prediction_DF$OUTCOME <- as.numeric(MortgageTree_Prediction_DF$OUTCOME)

MortgageTree_Prediction_ROCR <- ROCR::prediction(predictions = MortgageTree_Prediction_DF$MortgageTree_rF_Prediction,labels = MortgageTree_Prediction_DF$OUTCOME)
MortgageTree_Performance_ROCR <- performance(MortgageTree_Prediction_ROCR,"tpr","fpr")
plot(MortgageTree_Performance_ROCR, lty = 3, lwd = 3, main="ROCR Curve")
MortgageTree_auc_obj <- performance(MortgageTree_Prediction_ROCR, "auc" ) 
MortgageTree_auc_val <- unlist(slot(MortgageTree_auc_obj,"y.values"))

MortgageTree_MINauc = min(round(MortgageTree_auc_val, digits = 2))
MortgageTree_MAXauc = max(round(MortgageTree_auc_val, digits = 2))
MortgageTree_MINauct = paste(c("min(AUC) = "), MortgageTree_MINauc, sep = "")
MortgageTree_MAXauct = paste(c("max(AUC) = "), MortgageTree_MAXauc, sep = "")
legend(0.7, 0.5, c(MortgageTree_MINauct, MortgageTree_MAXauct, "\n"), border = "white", cex = 0.7, box.col = "white")
abline(a= 0, b=1)

#Lift chart

MortgageTree_Performance_Lift = performance(MortgageTree_Prediction_ROCR,"lift","rpp")
plot(MortgageTree_Performance_ROCR,main="lift chart")

#### Logistic Regression ####

logFit <- glm(OUTCOME ~ ., data = TrainingData, family=binomial(), maxit = 50)
summary(logFit) 

#Prediction matrix and Accuracy for the Training Data
log_Train_Probability <- predict(logFit, type = "response")
log_Train_Probability[1:10]
#contrasts(TrainingData$OUTOME)
log_Train_Pred <- rep("non-default",966)
log_Train_Pred[log_Train_Probability > 0.5] <- "default"

log_Train_Prediction <- table(log_Train_Pred, TrainingData$OUTCOME) 
log_Train_Prediction
log_Train_Accuracy <- sum(log_Train_Prediction[1,2] + log_Train_Prediction[2,1])/sum(log_Train_Prediction)
log_Train_Accuracy

#Prediction matrix and Accuracy for the Test Data
log_Test_Probability <- predict(logFit, newdata = TestData, type = "response")
log_Test_Probability[1:10]
#contrasts(TrainingData$OUTOME)
log_Test_Pred <- rep("non-default",329)
log_Test_Pred[log_Test_Probability > 0.5] <- "default"

log_Test_Prediction <- table(log_Test_Pred, TestData$OUTCOME) 
log_Test_Prediction
log_Test_Accuracy <- sum(log_Test_Prediction[1,2] + log_Test_Prediction[2,1])/sum(log_Test_Prediction)
log_Test_Accuracy

#Logistic Regression after Variable selection - Forward Selection

log_VarSel_TrainingData <- TrainingData
log_VarSel_TrainingData$OUTCOME <- ifelse(log_VarSel_TrainingData$OUTCOME == "default", 1, 0)

LogRnull <- lm(OUTCOME ~ 1,data = log_VarSel_TrainingData)
LogRfull <- lm(OUTCOME ~ .,data = log_VarSel_TrainingData) 
step(LogRnull, scope = list(lower = LogRnull, upper = LogRfull), direction = "forward")

log_VarSel <- glm(OUTCOME ~ Credit_score + pct_poverty + First_home + Tot_mthly_debt_exp, data = TrainingData, family=binomial(), maxit = 50)
summary(log_VarSel) 

#Prediction matrix and Accuracy for the Training Data
log_VarSel_Train_Probability <- predict(log_VarSel, type = "response")
log_VarSel_Train_Probability[1:10]
#contrasts(TrgA_TrainingData$OUTOME)
log_VarSel_Train_Pred <- rep("non-default",966)
log_VarSel_Train_Pred[log_VarSel_Train_Probability > 0.5] <- "default"

log_VarSel_Train_Prediction <- table(log_VarSel_Train_Pred, TrainingData$OUTCOME) 
log_VarSel_Train_Prediction
log_VarSel_Train_Accuracy <- sum(log_VarSel_Train_Prediction[1,2] + log_VarSel_Train_Prediction[2,1])/sum(log_VarSel_Train_Prediction)
log_VarSel_Train_Accuracy

#Prediction matrix and Accuracy for the Test Data
log_VarSel_Test_Probability <- predict(log_VarSel, newdata = TestData, type = "response")
log_VarSel_Test_Probability[1:10]
#contrasts(TrgB_TrainingData$OUTOME) 
log_VarSel_Test_Pred <- rep("non-default",329)
log_VarSel_Test_Pred[log_VarSel_Test_Probability > 0.5] <- "default"

log_VarSel_Test_Prediction <- table(log_VarSel_Test_Pred, TestData$OUTCOME) 
log_VarSel_Test_Prediction
log_VarSel_Test_Accuracy <- sum(log_VarSel_Test_Prediction[1,2] + log_VarSel_Test_Prediction[2,1])/sum(log_VarSel_Test_Prediction)
log_VarSel_Test_Accuracy

# Evaluation of model

#ROC Curve
install.packages("pROC")
library(pROC)

TestData_ROCR <- TestData
TestData_ROCR$prob <- log_VarSel_Test_Probability
log_ROCR <- roc(OUTCOME ~ prob, data = TestData_ROCR)
plot(log_ROCR, main = "ROC curve") 

#Lift chart

log_pred <- prediction(log_VarSel_Test_Probability, TestData$OUTCOME)
log_performance <- performance(log_pred, "lift", "rpp")
plot(log_performance, main = "Lift Chart", colorize = F)

#-------------------------------------------------------------------------

# Method 3: Neural Network
# -----------------------------------------------------------------------
# Constructing the neural network model on training dataset

library(nnet)
nn  = nnet(OUTCOME ~ ., data=TrainingData, linout=F, size=10, decay=0.01, maxit=1000)
summary(nn)

# Plotting the neural network
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

#plot each model
plot.nnet(nn, cex.val = 0.8, max.sp = T, circle.cex = 4)

# Getting the weights for the variables
nn$wts
nn$fitted.values

# Calculating performance metrics on training dataset
nn_pred_train <- predict(nn, TrainingData, type = "class")
nn.preds_training <- table(TrainingData$OUTCOME, nn_pred_train)
nn.preds_training

MortgageTrainingData_Accuracy_NN <- sum(nn.preds_training[1,2],nn.preds_training[2,1])/sum(nn.preds_training)
MortgageTrainingData_Accuracy_NN

MortgageTrainingData_TP_NN <- nn.preds_training[2,1]/sum(nn.preds_training[2,2]+nn.preds_training[2,1])
MortgageTrainingData_TP_NN

# Predicticting the variable in Test data
nn.preds <- predict(nn, TestData, type = "class")

# Calculating performance metrics on test dataset
confusion_matrix_NN <- table(TestData$OUTCOME, nn.preds)
confusion_matrix_NN

MortgageTestData_Accuracy_NN <- sum(confusion_matrix_NN[1,2],confusion_matrix_NN[2,1])/sum(confusion_matrix_NN)
MortgageTestData_Accuracy_NN

MortgageTestData_TP_NN <- confusion_matrix_NN[2,1]/sum(confusion_matrix_NN[2,2]+confusion_matrix_NN[2,1])
MortgageTestData_TP_NN

predictionDF_NN <- as.data.frame(nn.preds)
predictionDF_NN$OUTCOME <- TestData$OUTCOME
predictionDF_NN$nn.preds <- as.numeric(predictionDF_NN$nn.preds)
predictionDF_NN$OUTCOME <- as.numeric(predictionDF_NN$OUTCOME)

prediction_ROCR_NN <- ROCR::prediction(predictions = predictionDF_NN$nn.preds,labels = predictionDF_NN$OUTCOME)
performance_ROCR_NN <- performance(prediction_ROCR_NN,"tpr","fpr")
plot(performance_ROCR_NN, lty = 3, lwd = 3,main="ROCR Curve")
auc_obj_NN <- performance(prediction_ROCR_NN, "auc" )
auc_val_NN <- unlist(slot(auc_obj_NN,"y.values"))

minauc_NN = min(round(auc_val_NN, digits = 2))
maxauc_NN = max(round(auc_val_NN, digits = 2))
minauct_NN = paste(c("min(AUC) = "), minauc_NN, sep = "")
maxauct_NN = paste(c("max(AUC) = "), maxauc_NN, sep = "")
legend(0.7, 0.5, c(minauct_NN, maxauct_NN, "\n"), border = "white", cex = 0.7, box.col = "white")
abline(a= 0, b=1)

#Lift chart

performance_lift_NN = performance(prediction_ROCR_NN,"lift","rpp")
plot(performance_lift_NN,main="lift chart")

# ---------------- END -------------------------------------



