library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

#let's factor 
data_3 <- data_2
data_3$Sex <- factor(data_3$Sex)
data_3$Title <- factor(data_3$Title)
data_3$Deck <- factor(data_3$Deck)

set.seed(123)
trainIndex <- createDataPartition(data_3$Survived, p = 0.83, list = FALSE)
trainDataSplit <- data_3[trainIndex,]
testDataSplit <- data_3[-trainIndex,]


model <- rpart(Survived ~ Pclass + Sex + Age + FamilySize + Deck +Fare,
               data = trainDataSplit, method = "class")
# formula Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + Deck
# means predict Survived base on all these columns

rpart.plot(model) # plotting the tree

pred <- predict(model, testDataSplit, type = "class")
table(Predicted = pred, Actual = testDataSplit$Survived)
#           Actual
#Predicted  0  1
#         0 96 19
#         1 13 50

# let's see confusion matrix
confusionMatrix(pred, factor(testDataSplit$Survived))

#    Confusion Matrix and Statistics

#            Reference
#Prediction  0  1
#         0 96 19
#         1 13 50

#                  Accuracy : 0.8202          
#                    95% CI : (0.7558, 0.8737)
#       No Information Rate : 0.6124          
#       P-Value [Acc > NIR] : 1.645e-09       

#                     Kappa : 0.6152          

#    Mcnemar's Test P-Value : 0.3768          
                                          
#            Sensitivity : 0.8807          
#            Specificity : 0.7246          
#         Pos Pred Value : 0.8348          
#         Neg Pred Value : 0.7937          
#             Prevalence : 0.6124          
#         Detection Rate : 0.5393          
#   Detection Prevalence : 0.6461          
#      Balanced Accuracy : 0.8027          
                                          
#       'Positive' Class : 0     

importance <- varImp(model)
#Age      8.409125
#Deck    51.459601
#Fare    56.864880
#Parch   11.825167
#Pclass  59.539279
#Sex    104.921413
#SibSp   30.661609
#Title  113.773369   

# so, as we can see, Title, Sex, Class and Fare makes difference.
# we can interpret it as following (also using the tree diagram)
# according to the tree diagram and the importance table
# most likely to survive were women of higher classes who payed a lot for the fare
aggregate(Survived ~ Sex, data = trainDataSplit, mean)
#     Sex  Survived
#1 female 0.7450199
#2   male 0.1861472

aggregate(Survived ~ Pclass, data = trainDataSplit, mean)
#   Pclass  Survived
#1      1  0.6179775
#2      2  0.4609929
#3      3  0.2487310

# let's test on the test dataset
pred_2 <- predict(model, data_4, type = "class")
table(Predicted = pred)
data_final <- data.frame(test$PassengerId, pred_2)
colnames(data_final) <- c("PassengerId", "Survived")

write.csv(data_final, "/home/professor/dataScienceProjects/titanicSurvivorsCompetition/data/final.csv")

# ok, the prediction did not go that well, most probably we overfit our model

printcp(model)
plotcp(model)
best_cp <- model_cv$cptable[which.min(model$cptable[,"xerror"]), "CP"]
model_tuned <- prune(model_cv, cp = best_cp)
pred_3 <- predict(model_tuned, testDataSplit)
table(Predicted = pred_3)  # idk really


# maybe we should use caret
trainDataSplit$Survived <- factor(trainDataSplit$Survived, levels = c(0, 1))
testDataSplit$Survived <- factor(testDataSplit$Survived, levels = c(0, 1))# end=code survived/died as yes/no
train_control <- trainControl(method = "repeatedcv", # repeated cross validation
                              number = 10,
                              repeats = 5,
                              savePredictions = "final") # cross validation
tune_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.001))
#This creates a grid of values for the complexity parameter (cp) of the decision tree model.
#The model will be trained and evaluated with each cp value from 0.001 to 0.1 in increments of 0.001,
#to find the best hyperparameter
model_cv <- train(Survived ~ Pclass + Sex + SibSp + Fare,
                  data = trainDataSplit,
                  method = "rpart", # recursive partitioning aka decision tree
                  trControl = train_control, # use cross validation
                  tuneGrid = tune_grid)
print(model_cv)
pred_4 <- predict(model_cv, testDataSplit)
confusionMatrix(pred_4, testDataSplit$Survived)
importance_cv <- varImp(model_cv)
print(importance_cv)
plot(model_cv)

####
# 153th person in data_4 didn't pay, we need to fill it out
pred_test <- predict(model_cv, data_4)
table(Predicted = pred)
data_final <- data.frame(test$PassengerId, pred_test)
colnames(data_final) <- c("PassengerId", "Survived")
write.csv(data_final, "/home/professor/dataScienceProjects/titanicSurvivorsCompetition/data/final.csv")
