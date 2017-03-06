#rm(churn_TestData)
#rm(churn_TrainingData)
#rm(churnData_Norm)
#rm(churnData_backup)
#rm(churnData_Balanced)
#rm(churnData_Final)
#rm(churnData_NoOutlier)
#rm(churnData_Norm_NoOutlier)
#rm(churnnewData)

#### Read the Churn Dataset ####
churnData <- read.csv("ChurnData.csv", header = TRUE)
str(churnData)

##Checking for NULL values
sum(is.na(churnData$CustomerAge))

##Backup of the data
churnData_backup <- churnData
str(churnData_backup)

#### Introducing new columns for previous months ####
churnnewData <- churnData

churnnewData$CHIScoreMonth1 <- churnData$CHIScoreMonth0 - churnData$CHIScoreMonth0_1
churnnewData$SupportCasesMonth1 <- churnData$SupportCasesMonth0 - churnData$SupportCasesMonth0_1
churnnewData$SPMonth1 <- churnData$SPMonth0 - churnData$SPMonth0_1

str(churnnewData)

#### Normalizing the dataset ####
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
churnData_Norm <- churnnewData

churnData_Norm$CustomerAge <- normalize(churnnewData$CustomerAge)
churnData_Norm$CHIScoreMonth0 <- normalize(churnnewData$CHIScoreMonth0)
churnData_Norm$CHIScoreMonth1 <- normalize(churnnewData$CHIScoreMonth1)
churnData_Norm$CHIScoreMonth0_1 <- normalize(churnnewData$CHIScoreMonth0_1)
churnData_Norm$SupportCasesMonth0 <- normalize(churnnewData$SupportCasesMonth0)
churnData_Norm$SupportCasesMonth1 <- normalize(churnnewData$SupportCasesMonth1)
churnData_Norm$SupportCasesMonth0_1 <- normalize(churnnewData$SupportCasesMonth0_1)
churnData_Norm$SPMonth0 <- normalize(churnnewData$SPMonth0)
churnData_Norm$SPMonth1 <- normalize(churnnewData$SPMonth1)
churnData_Norm$SPMonth0_1 <- normalize(churnnewData$SPMonth0_1)
churnData_Norm$Logins0_1 <- normalize(churnnewData$Logins0_1)
churnData_Norm$LogArticles0_1 <- normalize(churnnewData$LogArticles0_1)
churnData_Norm$Views0_1 <- normalize(churnnewData$Views0_1)
churnData_Norm$DaysSinceLastLogin0_1 <- normalize(churnnewData$DaysSinceLastLogin0_1)

##### Checking for Outliers ####
churnData_NoOutlier <- churnData_Norm

Outlier <- boxplot(churnData_Norm$CustomerAge)$out
churnData_NoOutlier$CustomerAge <- ifelse(churnData_NoOutlier$CustomerAge == 0, NA, churnData_NoOutlier$CustomerAge)

Outlier <- boxplot(churnData_Norm$CHIScoreMonth0)$out
Outlier <- boxplot(churnData_Norm$CHIScoreMonth1)$out
Outlier <- boxplot(churnData_Norm$CHIScoreMonth0_1)$out
Outlier <- boxplot(churnData_Norm$SupportCasesMonth0)$out
Outlier <- boxplot(churnData_Norm$SupportCasesMonth1)$out
Outlier <- boxplot(churnData_Norm$SupportCasesMonth0_1)$out
Outlier <- boxplot(churnData_Norm$SPMonth0)$out
Outlier <- boxplot(churnData_Norm$SPMonth1)$out
Outlier <- boxplot(churnData_Norm$SPMonth0_1)$out
Outlier <- boxplot(churnData_Norm$Logins0_1)$out
Outlier <- boxplot(churnData_Norm$LogArticles0_1)$out

Outlier <- boxplot(churnData_Norm$Views0_1)$out
churnData_NoOutlier$Views0_1 <- ifelse(churnData_NoOutlier$Views0_1 >= 0.95, NA, churnData_NoOutlier$Views0_1)

Outlier <- boxplot(churnData_Norm$DaysSinceLastLogin0_1)$out
churnData_NoOutlier$DaysSinceLastLogin0_1 <- ifelse(churnData_NoOutlier$DaysSinceLastLogin0_1 <= 0.05, NA, churnData_NoOutlier$DaysSinceLastLogin0_1)

##Removing Outliers
churnData_Final <- na.omit(churnData_NoOutlier)
str(churnData_Final)

#### Imbalanced Sampling ####
##Performing both under-sampling and over-sampling
library("ROSE")
churnData_Balanced <- ovun.sample(Churn ~ ., data = churnData_Final, method = "both", p=0.5, N=3000, seed = 2014)$data
table(churnData_Balanced$Churn)
prop.table(table(churnData_Balanced$Churn))

#churnData_Balanced$Churn <- as.factor(churnData_Balanced$Churn)

#### Splitting balanced Churn Dataset into Training (75%) and Testing (25%) data ####
set.seed(2014)
ind <- sample(2, nrow(churnData_Balanced), replace=TRUE, prob=c(0.75, 0.25))
churn_TrainingData <- churnData_Balanced[ind==1,]
churn_TestData <- churnData_Balanced[ind==2,]

table(churn_TrainingData$Churn) #2248 (0-1095; 1-1153)
prop.table(table(churn_TrainingData$Churn))

table(churn_TestData$Churn) #752 (0-362; 1-390)
prop.table(table(churn_TestData$Churn))

#### Logistic Regression ####
log_churn <- glm(Churn ~ . - ID - CHIScoreMonth0_1 - SupportCasesMonth0_1 - SPMonth0_1, data = churn_TrainingData, family=binomial(), maxit = 500)
summary(log_churn)

#### Performance metrics for Training Dataset ####

#Prediction of Training Dataset
log_churnTrain_Prob <- predict(log_churn, type = "response")
#log_churnTrain_Prob[1:25]
#contrasts(churn_TrainingData$Churn)
log_churnTrain_Pred <- rep("No-Churn", 2248)
log_churnTrain_Pred[log_churnTrain_Prob > 0.5] <- "Churn"

log_churnTrain_Prediction <- table(log_churnTrain_Pred, churn_TrainingData$Churn) 
log_churnTrain_Prediction

#Accuracy of the Training Dataset
log_churnTrain_Accuracy <- sum(log_churnTrain_Prediction[1,2] + log_churnTrain_Prediction[2,1]) / sum(log_churnTrain_Prediction)
log_churnTrain_Accuracy

#TP Rate of the Training Dataset
log_churnTrain_TPRate <- log_churnTrain_Prediction[1,2] / sum(log_churnTrain_Prediction[1,2] + log_churnTrain_Prediction[2,2])
log_churnTrain_TPRate

#### Performance metrics for Test Dataset ####

#Prediction of Test Dataset
log_churnTest_Prob <- predict(log_churn, newdata = churn_TestData, type = "response")
#contrasts(churn_TrainingData$Churn)
log_churnTest_Pred <- rep("No-Churn", 752)
log_churnTest_Pred[log_churnTest_Prob > 0.5] <- "Churn"

log_churnTest_Prediction <- table(log_churnTest_Pred, churn_TestData$Churn) 
log_churnTest_Prediction

#Accuracy of the Test Dataset
log_churnTest_Accuracy <- sum(log_churnTest_Prediction[1,2] + log_churnTest_Prediction[2,1]) / sum(log_churnTrain_Prediction)
log_churnTest_Accuracy

#TP Rate of the Test Dataset
log_churnTest_TPRate <- log_churnTest_Prediction[1,2] / sum(log_churnTest_Prediction[1,2] + log_churnTest_Prediction[2,2])
log_churnTest_TPRate

##### Forward Selection ####

LogRnull <- lm(Churn ~ 1,data = churn_TrainingData)
LogRfull <- lm(Churn ~ . - ID - CHIScoreMonth0_1 - SupportCasesMonth0_1 - SPMonth0_1, data = churn_TrainingData) 
step(LogRnull, scope = list(lower = LogRnull, upper = LogRfull), direction = "forward")

logVarSel_churn <- glm(Churn ~ CHIScoreMonth0 + CHIScoreMonth1 + DaysSinceLastLogin0_1 + CustomerAge + Views0_1 + SupportCasesMonth1 + LogArticles0_1, data = churn_TrainingData, family=binomial(), maxit = 300)
summary(logVarSel_churn) 

#### Performance metrics for Test Dataset after Variable Selection ####

#Prediction of Test Dataset after Variable Selection
logVarSel_churnTest_Prob <- predict(logVarSel_churn, newdata = churn_TestData, type = "response")
#logVarSel_churnTest_Prob[1:25]
#contrasts(churn_TrainingData$Churn)
logVarSel_churnTest_Pred <- rep("No-Churn", 752)
logVarSel_churnTest_Pred[logVarSel_churnTest_Prob > 0.5] <- "Churn"

logVarSel_churnTest_Prediction <- table(logVarSel_churnTest_Pred, churn_TestData$Churn) 
logVarSel_churnTest_Prediction

#Accuracy of the Test Dataset after Variable Selection
logVarSel_churnTest_Accuracy <- sum(logVarSel_churnTest_Prediction[1,2] + logVarSel_churnTest_Prediction[2,1]) / sum(logVarSel_churnTest_Prediction)
log_churnTest_Accuracy

#TP Rate of the Test Dataset after Variable Selection
logVarSel_churnTest_TPRate <- logVarSel_churnTest_Prediction[1,2] / sum(logVarSel_churnTest_Prediction[1,2] + logVarSel_churnTest_Prediction[2,2])
logVarSel_churnTest_TPRate


#### Top 100 Customers having the highest Churn Probabilities ####

logVarSel_churnData_Prob <- predict(logVarSel_churn, newdata = churnData_Final, type = "response")
ChurnProb <- as.data.frame(logVarSel_churnData_Prob)
ChurnData_results <- cbind(churnData_Final, ChurnProb)

#str(ChurnData_results)
#head(ChurnData_results, n=5)

Churn_OrderedProb <- ChurnData_results[order(-logVarSel_churnData_Prob),] 
Churn_Top100 <- head(Churn_OrderedProb, n=100)
write.csv(Churn_Top100, file = "Top_100_Customers_Churn.csv")
write.csv(Churn_OrderedProb, file = "Churn.csv")
Churn_Top100$ID

##CustomerAge + CHIScoreMonth0 + CHIScoreMonth0_1 + SupportCasesMonth0 + SupportCasesMonth0_1 + SPMonth0 + SPMonth0_1 + Logins0_1 + LogArticles0_1 + Views0_1 + DaysSinceLastLogin0_1 + Churn



