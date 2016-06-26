setwd("~/Desktop/GW_spring2016/data analysis/project 2")
library(reshape)
library(ggplot2)
library(caret)
library(randomForest)
library(MASS)
library(data.table)
library(bit64)
library(corrplot)
library(plyr)
library(ROCR)
library(cluster)
library(ROSE)
library(car)
library(ROCR)
library(bestglm)
library(klaR)
library(glmnet)
library(tsne)
library(mgcv)
options(scipen = 999)

############################################################
setwd("~/Desktop/practicum/venga_practicum")

#Part 1: Data Exploration: dataset is the Santander dataset used throughout HW6 and this project.

##functions
same_colnames <- function(df1,df2){
  colnames <- colnames(df1)
  colnames.same <- df2[,-which(colnames(df1) %in% colnames)]
  print(names(colnames.same)) 
}
same_colnames(model.step.train,model.step)

#define RMSE function
RMSE <- function(predicted, true) mean((predicted-true)^2)^.5


########FACT TABLE: create and split into train and test
fact.table <- as.data.frame(fread("fact_table.csv"))
str(fact.table)
#remove V1 and X
fact.table$V1 <- NULL
fact.table$X <- NULL

###two target variables: number of repeats for a continuous model and
###repeat - yes or no for categorical. will split these out as needed after initial dimenstionality
###reduction.


#####split into train and test set
##we will be using cross validation but also keeping aside .1 of data to validate on to 
#test accuracy of both methods
row <-nrow(fact.table)
trainindex <- sample(row, 0.9*row, replace=FALSE)
train <- fact.table[trainindex,]
test  <- fact.table[-trainindex,]

set.seed(12345)

#separate out near zero variance predictors. this may not be accurate because of the categorical predictors
#but lets look
nzv <- nearZeroVar(train)
look.nzv <- train[,nzv]
#looks like more so the variables are sparsley populated...

#example: just a bunch of outliers
dev.off()
boxplot(x = look.nzv[,30])
#TO DO***plot all of the near zero variables in a facet wrap with variable names. 
#can decide which ones to keep and maybe which models to keep them in*****

#for now, remove near zero var predictors:
train.var <- train[-nzv]
test.var <- test[-nzv]

#convert needed variables (all flag and a couple other) to factors in train.var
str(train.var)
train.factor<- train.var[ , grepl( "flag" , names(train.var) ) ]
train.factor <- as.data.frame(sapply(train.factor, as.factor))
train.var <- train.var[,-which(names(train.var) %in% names(train.factor))]
train.ready <- cbind(train.var,train.factor)
train.ready$last_server_name <- as.factor(train.ready$last_server_name)
train.ready$first_server_name <- as.factor(train.ready$first_server_name)

#and for test.var
str(test.var)
test.factor<- test.var[ , grepl( "flag" , names(test.var) ) ]
test.factor <- as.data.frame(sapply(test.factor, as.factor))
test.var <- test.var[,-which(names(test.var) %in% names(test.factor))]
test.ready <- cbind(test.var,test.factor)
test.ready$last_server_name <- as.factor(test.ready$last_server_name)
test.ready$first_server_name <- as.factor(test.ready$first_server_name)

#correlation (only numeric variables)
str(train.ready)
#separate out all numeric variables
train.corr <- train.ready[sapply(train.ready, is.numeric)]
#remove loyalty user id
train.corr$loyalty_user_id <- NULL
#calculate correlation matrix
descrCor <- cor(train.corr)
#look at max correlation
summary(descrCor[upper.tri(descrCor)])

#next step is to remove redundant variables by means of highly correlated variables
#look at corrplot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
highlyCorrelated <- findCorrelation(descrCor, cutoff = .9)
#24 variables that are over .9 correlated
look.corr<- train.corr[,highlyCorrelated]
#these are highly orrelated
highlyCorCol <- colnames(train.corr)[highlyCorrelated]
highlyCorCol
#all of these are either combos of first/last or min/max/avg...we should maybe decide on which ones to keep. for now,
#i am leaving them in the data set, i think they are important enough and we should keep on of the two/three from each group of highly
#correlated variables

#random forest on the intial data set to look at important variables. will first run with 1/0 target:
prop.table(table(train.ready$has.repeated))
#unbalanced target, we can use undersampling method to balance target, but first lets use rf and see its performance
#target to factor:
train.ready$has.repeated <- as.factor(train.ready$has.repeated)
test.ready$has.repeated <- as.factor(test.ready$has.repeated)
train.rf <- train.ready
train.rf$num_of_repeats <- NULL
train.rf$loyalty_user_id <- NULL
#had to remove first and last server name bc the levels were inconsistent between train and test sets
train.rf$last_server_name <- NULL
train.rf$first_server_name <- NULL

test.rf <- test.ready
test.rf$num_of_repeats <- NULL
test.rf$loyalty_user_id <- NULL
#had to remove first and last server name bc the levels were inconsistent between train and test sets
test.rf$last_server_name <- NULL
test.rf$first_server_name <- NULL

rf <- randomForest(has.repeated ~ ., data = train.rf,importance=TRUE,ntree=100)
rf$confusion
imp <- importance(rf,type=1)
imp <- data.frame(predictors=rownames(imp),imp)
imp.sort <- arrange(imp,desc(MeanDecreaseAccuracy))
imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)
varImpPlot(rf, type=1)
imp.20 <- imp.sort[1:20,]
#see the highly correlated variables really do no affect the prediction as much

#predict on test
predictions <- as.data.frame(predict(rf,test.rf,type="response"))
table(predictions)

#ROC and AUC
pred.forest <- predict(rf,type="prob",newdata=test.rf)
pred.forest.roc <- prediction(pred.forest[,2],test.rf$has.repeated)
pred.forest.performance <- performance(pred.forest.roc,"tpr","fpr")
plot(pred.forest.performance, col=3)
performance(pred.forest.roc,"auc")@y.values[[1]] #AUC

##??? what is happening...i am removing the highly correlated variables
train.rf1 <- train.rf[,-which(colnames(train.rf) %in% highlyCorCol)]
test.rf1 <- test.rf[,-which(colnames(test.rf) %in% highlyCorCol)]
same_colnames(train.rf1,test.rf1)
rf1 <- randomForest(has.repeated ~ ., data = train.rf1,importance=TRUE,ntree=100)
rf1$confusion
predictions1 <- as.data.frame(predict(rf1,test.rf1,type="response"))
table(predictions1)
