# Set Working Directory Based on your devices
getwd()
setwd("/Users/Juliet/Documents/Monash_Uni/FIT3152/Assignment/2")

# Install Packages (If you have not have these packages)
install.packages("tree")
install.packages("e1071")
install.packages("ROCR")
install.packages("randomForest")
install.packages("adabag")
install.packages("rpart")

# Load the Packages
library(tree)
library(e1071)
library(ROCR)
library(randomForest)
library(adabag)
library(rpart)

# Create individual data based on requirements
rm(list = ls())
WAUS <- read.csv("WarmerTomorrow2022.csv")
L <- as.data.frame(c(1:49))
set.seed(32112602) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

# 1
# Find the unique values of Warmer Tomorrow
unique(WAUS$WarmerTomorrow) 

# NA values are present. It needs to be omitted
WAUS <- subset(WAUS, is.na(WAUS$WarmerTomorrow) == FALSE)

# Check the unique values once more
# Only 1 and 0 unique values are present. Thus, NA values has successfully omitted
unique(WAUS$WarmerTomorrow) 

# Proportion of days when it is warmer than the previous day compared to those where it is cooler
warmer.pp <- round(nrow(WAUS[WAUS$WarmerTomorrow == 1,])/nrow(WAUS)*100, 2) # 54.79%
paste(warmer.pp,"%", sep = '')

# Description of predictor (independent) variables
round(sapply(WAUS, sd, na.rm = TRUE),4)
summary(WAUS)

# 2 
# Drop Day, Month, Year columns as they are not relevant to our model.
WAUS <- subset(WAUS, select = -c(Day, Month, Year))

# Delete all rows that have null values in the column
WAUS <- WAUS[complete.cases(WAUS),]
dim(WAUS)

# Change chr type and WarmerTomorrow column into Factor
str(WAUS)
WAUS[c("WindGustDir", "WindDir9am", "WindDir3pm","WarmerTomorrow")] <- lapply(WAUS[c("WindGustDir", "WindDir9am", "WindDir3pm","WarmerTomorrow")], as.factor)
str(WAUS)

# 3
# Divide 70% train data - 30% test data
set.seed(32112602) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.train = WAUS[train.row,]
WAUS.test = WAUS[-train.row,]

# 4
# Decision Tree
WAUS.tree <- tree(WarmerTomorrow~., data = WAUS.train)
plot(WAUS.tree)
text(WAUS.tree, pretty = 0)
summary(WAUS.tree)

# Naive Bayes
WAUS.bayes <- naiveBayes(WarmerTomorrow~., data = WAUS.train)

# Bagging
WAUS.bag <- bagging(WarmerTomorrow~., data = WAUS.train, mfinal = 10)

# Boosting
WAUS.boost <- boosting(WarmerTomorrow~., data = WAUS.train, mfinal= 10)

# Random Forest
WAUS.rf <- randomForest(WarmerTomorrow~., data = WAUS.train)

# 5
# Decision Tree
WAUS.pred.tree <- predict(WAUS.tree, WAUS.test, type = 'class')
WAUS.pred.tree
dt.t <- table(predicted = WAUS.pred.tree, actual = WAUS.test$WarmerTomorrow)
dt.t

dt.a <- round((60+75)/nrow(WAUS.test),4)
dt.a
# Accuracy = (60+75)/nrow(WAUS.test) = 0.6308

# Naive Bayes
WAUS.predbayes <- predict(WAUS.bayes, WAUS.test)
nb.t <- table(predicted = WAUS.predbayes, actual = WAUS.test$WarmerTomorrow)
nb.t

nb.a <- round((64+88)/nrow(WAUS.test),4)
nb.a
# Accuracy =(64+88)/nrow(WAUS.test) = 0.7103

# Bagging 
WAUSpred.bag <- predict.bagging(WAUS.bag, WAUS.test)
WAUSpred.bag$confusion
bag.a <- round((53+92)/nrow(WAUS.test),4)
bag.a
# Accuracy =(53+92)/nrow(WAUS.test) = 0.6776

# Boosting
WAUSpred.boost <- predict.boosting(WAUS.boost, WAUS.test)
WAUSpred.boost$confusion
boo.a <- round((61+86)/nrow(WAUS.test),4)
boo.a
# Accuracy =(61+86)/nrow(WAUS.test) = 0.6869

# Random Forest
WAUSpred.rf <- predict(WAUS.rf, WAUS.test)
rf.t <- table(predicted = WAUSpred.rf, actual = WAUS.test$WarmerTomorrow)
rf.t

rf.a <- round((59+95)/nrow(WAUS.test),4)
rf.a 
# Accuracy = (59+95)/nrow(WAUS.test) = 0.7196

# 6
# Decision Tree
# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve
WAUS.pred.tree <- predict(WAUS.tree, WAUS.test, type = 'vector')
WAUSdpred <- prediction(WAUS.pred.tree[,2], WAUS.test$WarmerTomorrow)
WAUSdperf <- performance(WAUSdpred, 'tpr','fpr')
plot(WAUSdperf)
abline(0,1)

cauc.dt <- performance(WAUSdpred, "auc")
dt.auc <- round(as.numeric(cauc.dt@y.values),4)
dt.auc
# AUC = 0.681

# Naive Bayes
# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve
WAUSpred.bayes <- predict(WAUS.bayes, WAUS.test, type ='raw')
WAUSbpred <- prediction(WAUSpred.bayes[,2], WAUS.test$WarmerTomorrow)
WAUSbperf <- performance(WAUSbpred, 'tpr','fpr')
plot(WAUSbperf, add = TRUE, col = 'blueviolet')

cauc.nb <- performance(WAUSbpred, "auc")
nb.auc <- round(as.numeric(cauc.nb@y.values),4)
nb.auc
# AUC = 0.739

# Bagging
# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve
WAUSbagpred <- prediction(WAUSpred.bag$prob[,2], WAUS.test$WarmerTomorrow)
WAUSbagperf <- performance(WAUSbagpred, 'tpr','fpr')
plot(WAUSbagperf, add = T, col = 'blue')

cauc.bag <- performance(WAUSbagpred, "auc")
bag.auc <- round(as.numeric(cauc.bag@y.values),4)
bag.auc
# AUC = 0.7029

# Boosting
# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve
WAUSboostpred <- prediction(WAUSpred.boost$prob[,2], WAUS.test$WarmerTomorrow)
WAUSboostperf <- performance(WAUSboostpred, 'tpr','fpr')
plot(WAUSboostperf, add = T, col = 'red')

cauc.boo <- performance(WAUSboostpred, "auc")
boo.auc <- round(as.numeric(cauc.boo@y.values),4)
boo.auc
# AUC = 0.7491

# Random Forest
# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve
WAUSrfpred <- predict(WAUS.rf, WAUS.test, type = "prob")
WAUSpred <- prediction(WAUSrfpred[,2], WAUS.test$WarmerTomorrow)
WAUSrfperf <- performance(WAUSpred, 'tpr','fpr')
plot(WAUSrfperf, add = T, col = 'green')

cauc.rf <- performance(WAUSpred, "auc")
rf.auc <- round(as.numeric(cauc.rf@y.values),4)
rf.auc
# AUC = 0.7803

# Completing ROC curve graph
title("ROC of Multiple Classifiers")
legend("topleft",
       legend=c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"),
       col=c("black",  "blueviolet","blue", "red","green"),
       lwd=4, cex =0.5, xpd = FALSE, horiz = FALSE)

# 7
# Create comparison table for Accuracy and AUC for each classifiers
table.compare <- data.frame(Classifier = c("Decision Tree", "Naives Bayes", "Bagging","Boosting","Random Forest"), Accuracy = c(dt.a, nb.a, bag.a, boo.a, rf.a), AUC = c(dt.auc, nb.auc, bag.auc, boo.auc, rf.auc))
View(table.compare)

# 8 - Variable Importance
# Decision Tree
summary(WAUS.tree)
WAUS.bag$importance[order(WAUS.bag$importance, decreasing = TRUE)]
WAUS.boost$importance[order(WAUS.boost$importance, decreasing = TRUE)]
WAUS.rf$importance[order(WAUS.rf$importance, decreasing = TRUE),]

# 9  - Create Simple Model
test.simple.fit <- cv.tree(WAUS.tree, FUN = prune.misclass)
print(test.simple.fit)
# Should have choose size 30 due to the lowest misclassification rate . But for simplicity of the graph, use smaller size number ranging from 2-5.
# The lowest misclassification rate from size 1-5 is 3.
prune.WAUS.fit <- prune.misclass(WAUS.tree, best = 3)
summary(prune.WAUS.fit)
plot(prune.WAUS.fit)
text(prune.WAUS.fit, pretty = 0)

WAUS.simple.predict <- predict(prune.WAUS.fit, WAUS.test, type = 'class')
WAUS.simple.t<- table(predicted = WAUS.simple.predict, actual = WAUS.test$WarmerTomorrow)
print(WAUS.simple.t)

dt.simple.a <- round((47 + 91)/nrow(WAUS.test),4)
dt.simple.a
# Accuracy = (47 + 91)/nrow(WAUS.test) = 0.6449

# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve of multiple classifiers
WAUS.simple.pred.tree <- predict(prune.WAUS.fit, WAUS.test, type = 'vector')
WAUS.simple.dpred <- prediction(WAUS.simple.pred.tree[,2], WAUS.test$WarmerTomorrow)
WAUS.simple.dperf <- performance(WAUS.simple.dpred, 'tpr','fpr')
plot(WAUSdperf)
plot(WAUSbperf, add = T, col = 'blueviolet')
plot(WAUSbagperf, add = T, col = 'blue')
plot(WAUSboostperf, add = T, col = 'green')
plot(WAUSrfperf, add = T, col = 'yellow')
plot(WAUS.simple.dperf, add = T, col = "pink")
title("ROC of Multiple Classifiers")
legend("topleft",
       legend=c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest", "Simple Model"),
       col=c("black",  "blueviolet","blue", "green","yellow","pink"),
       lwd=4, cex =0.5, xpd = FALSE, horiz = FALSE)
abline(0,1)

cauc.simple.dt <- performance(WAUS.simple.dpred, "auc")
dt.simple.auc <- round(as.numeric(cauc.simple.dt@y.values),4)
dt.simple.auc
# AUC = 0.6675

# Insert the accuracy and AUC simple model's values into table.compare
simple.values <- c("Simple Model", dt.simple.a, dt.simple.auc)
table.compare <- rbind(table.compare, simple.values)
View(table.compare)

# 10 - Best Classifier
# Choose to improve RF since it has the highest Accuracy and AUC out of the other classifiers in Q7
set.seed(32112602)
WAUS.rf.imp <- randomForest(WarmerTomorrow~.-Location, data = WAUS.train)
WAUSpred.rf.imp <- predict(WAUS.rf.imp, WAUS.test)
rf.t.imp <- table(predicted = WAUSpred.rf.imp, actual = WAUS.test$WarmerTomorrow)
rf.t.imp

rf.a.imp <- round((58+100)/nrow(WAUS.test),4)
rf.a.imp
# Accuracy = (58+100)/nrow(WAUS.test) = 0.7383

WAUSrfpred.imp <- predict(WAUS.rf.imp, WAUS.test, type = "prob")
WAUSpred.imp <- prediction(WAUSrfpred.imp[,c("1")], WAUS.test$WarmerTomorrow)
cauc.rf.imp <- performance(WAUSpred.imp, "auc")
rf.auc.imp <- round(as.numeric(cauc.rf.imp@y.values),4)
rf.auc.imp
# AUC = 0.786

# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve of multiple classifiers
WAUS.rf.perf <- performance(WAUSpred.imp, 'tpr','fpr')
plot(WAUSdperf)
plot(WAUSbperf, add = T, col = 'blueviolet')
plot(WAUSbagperf, add = T, col = 'blue')
plot(WAUSboostperf, add = T, col = 'green')
plot(WAUSrfperf, add = T, col = 'yellow')
plot(WAUS.rf.perf, add = T, col="red")
abline(0,1)
title("ROC of Multiple Classifiers")
legend("topleft",
       legend=c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest", "Best Random Forest"),
       col=c("black",  "blueviolet","blue", "green","yellow","red"),
       lwd=4, cex =0.5, xpd = FALSE, horiz = FALSE)

# Insert the accuracy and AUC best classifier's values into table.compare
best.values <- c("Best Random Forest (Best Classifier)", rf.a.imp, rf.auc.imp)
table.compare <- rbind(table.compare, best.values)
View(table.compare)

# 11 - ANN
install.packages('neuralnet')
install.packages('car')

library(neuralnet)
library(car)

# Create individual data based on requirements
rm(list = ls())
WAUS <- read.csv("WarmerTomorrow2022.csv")
L <- as.data.frame(c(1:49))
set.seed(32112602) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows

# Delete the rows that have null values of WarmerTomorrow
WAUS <- subset(WAUS, is.na(WAUS$WarmerTomorrow) == FALSE)

# Drop Day, Month, Year, Location columns as they are not relevant to our model and deletion of Location column are proved to improve the accuracy of best classifier random forest
WAUS <- subset(WAUS, select = -c(Day, Month, Year, Location))

# Delete all rows that have null values
WAUS <- WAUS[complete.cases(WAUS),]
dim(WAUS)

# Create a model matrix for chr type columns
WAUS.mm <- model.matrix(~WindGustDir+WindDir9am+WindDir3pm,data= WAUS)
WAUS <- cbind(WAUS ,WAUS.mm)
WAUS <- subset(WAUS, select = -c(WindGustDir, WindDir9am, WindDir3pm, `(Intercept)`))

# Divide 70% Train Data - 30% Test Data
set.seed(32112602) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
WAUS.nn.train = WAUS[train.row,]
WAUS.nn.test = WAUS[-train.row,]

# Build ANN model (May take some time)
WAUS.nn <- neuralnet(WarmerTomorrow~MinTemp + MaxTemp + Rainfall + Evaporation + Sunshine + WindGustSpeed + WindSpeed9am + WindSpeed3pm + Humidity9am + Humidity3pm + Pressure9am+ Pressure3pm + Cloud9am + Cloud3pm + Temp9am + Temp3pm + WindGustDirENE + WindGustDirESE + WindGustDirN + WindGustDirNE + WindGustDirNNE + WindGustDirNNW+WindGustDirNW + WindGustDirS + WindGustDirSE + WindGustDirSSE + WindGustDirSSW + WindGustDirSW + WindGustDirW + WindGustDirWNW + WindGustDirWSW + WindDir9amENE + WindDir9amESE + WindDir9amN + WindDir9amNE + WindDir9amNNE + WindDir9amNNW + WindDir9amNW + WindDir9amS + WindDir9amSE + WindDir9amSSE + WindDir9amSSW + WindDir9amSW + WindDir9amW + WindDir9amWNW + WindDir9amWSW + WindDir3pmENE + WindDir3pmESE + WindDir3pmN + WindDir3pmNE + WindDir3pmNNE + WindDir3pmNNW + WindDir3pmNW + WindDir3pmS + WindDir3pmSE + WindDir3pmSSE + WindDir3pmSSW + WindDir3pmSW + WindDir3pmW + WindDir3pmWNW + WindDir3pmWSW , data= WAUS.nn.train,hidden = 25, linear.output = FALSE)

# Predict 
WAUS.nn.pred <-compute(WAUS.nn, WAUS.nn.test[,c("MinTemp","MaxTemp","Rainfall", "Evaporation" , "Sunshine" , "WindGustSpeed" , "WindSpeed9am" , "WindSpeed3pm" , "Humidity9am" , "Humidity3pm" , "Pressure9am", "Pressure3pm" , "Cloud9am" , "Cloud3pm" , "Temp9am" , "Temp3pm" , "WindGustDirENE" , "WindGustDirESE" , "WindGustDirN" , "WindGustDirNE" , "WindGustDirNNE" , "WindGustDirNNW","WindGustDirNW" , "WindGustDirS" , "WindGustDirSE" , "WindGustDirSSE" , "WindGustDirSSW" , "WindGustDirSW" , "WindGustDirW" , "WindGustDirWNW" , "WindGustDirWSW", "WindDir9amENE" , "WindDir9amESE" , "WindDir9amN" , "WindDir9amNE" , "WindDir9amNNE" , "WindDir9amNNW" , "WindDir9amNW" , "WindDir9amS" , "WindDir9amSE" , "WindDir9amSSE" , "WindDir9amSSW" , "WindDir9amSW" , "WindDir9amW" , "WindDir9amWNW" , "WindDir9amWSW" , "WindDir3pmENE" , "WindDir3pmESE" , "WindDir3pmN", "WindDir3pmNE" , "WindDir3pmNNE" , "WindDir3pmNNW" , "WindDir3pmNW" , "WindDir3pmS" , "WindDir3pmSE" , "WindDir3pmSSE" , "WindDir3pmSSW" , "WindDir3pmSW" , "WindDir3pmW" , "WindDir3pmWNW" , "WindDir3pmWSW")])
WAUS.nn.pred <- ifelse(WAUS.nn.pred$net.result >= 0.5,1,0)
table(actual = WAUS.nn.test$WarmerTomorrow, prediction = WAUS.nn.pred)

nn.a<-round((62+76)/nrow(WAUS.nn.test),4)
nn.a
# Accuracy (62+76)/nrow(WAUS.nn.test) = 0.6449

WAUSpred.nn <- predict(WAUS.nn, WAUS.nn.test, type = "prob")
detach(package:neuralnet,unload = T)
WAUSpred.nn <- prediction(WAUSpred.nn, WAUS.nn.test$WarmerTomorrow)
cauc.nn <- performance(WAUSpred.nn, "auc")
nn.auc <- round(as.numeric(cauc.nn@y.values),4)
nn.auc
# AUC = 0.677

# Calculate the confidence of predicting ‘warmer tomorrow’ and construct an ROC curve
WAUS.nn.perf <- performance(WAUSpred.nn, 'tpr','fpr')
plot(WAUS.nn.perf, col="red", main="ROC of ANN")
abline(0,1)