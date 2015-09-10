########################################################################################
# Name: Sandeep Pidshetti
# Team : DS450-Team #3 
# DS450 Course - Digit Recognizer Kaggle Competition
# Model: Average Random Forest 
# Initial score :0.89414
# Best score:0.96329
#######################################################################################
# Clear workspace and console
rm(list=ls())
cat("\014")

########################################################################################
# Load training set and replace pixel values with "1" for value >0

training.set = read.csv("C:\\Users\\dellxps\\Desktop\\DataScience\\DS450\\kaggle\\data_train_minus_holdout.csv", header=TRUE)
label<-training.set$label
training.set[training.set > 0]<- 1
training.set$label<-label

########################################################################################
# Use subset of training data set to train the model and remaining data for testing purpose
library(caTools)
split = sample.split(training.set$label, SplitRatio=.9)
train = subset(training.set, split==TRUE)
test = subset(training.set, split==FALSE)

# Use RandomForest Algorithm 
library(randomForest)
train$label = factor(train$label)
test$label = factor(test$label)
combi <- rbind(train,test)

# Get average of three runs of RandomForest 
randomForest_1 = randomForest(label ~ ., data=combi, nodesize=5,ntree=200,do.trace = F)
randomForest_2 = randomForest(label ~ ., data=combi, nodesize=5,ntree=200,do.trace = F)
randomForest_3 = randomForest(label ~ ., data=combi, nodesize=5,ntree=200,do.trace = F)
avgRF.all <- combine(randomForest_1,randomForest_2,randomForest_3)
 

# Load real test data and replace pixel values with "1" for value >0
test.kaggle<-read.csv("C:\\Users\\dellxps\\Desktop\\DataScience\\DS450\\kaggle\\test.csv")
label<-test.kaggle$label
test.kaggle[test.kaggle > 0]<- 1
test.kaggle$label<-label

# Run the model against real test data 
randomForestPredict = predict(avgRF.all,test.kaggle)
predictions <- data.frame(ImageId = 1:nrow(test.kaggle),Label = randomForestPredict)

# Write final prediction output to a file
write.csv(predictions, file = "C:\\Users\\dellxps\\Desktop\\DataScience\\DS450\\kaggle\\myowndata\\predictions.csv",row.names = F)

# Cross validation to check the score
score=mean(predictions[,2]==randomForestPredict)
print(sprintf("Final score: %f",score))
print("------------Complete--------------------------------------------------")

########################################################################################

