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
trainIndex <- createDataPartition(data_3$Survived, p = 0.8, list = FALSE)
trainDataSplit <- data_3[trainIndex,]
testDataSplit <- data_3[-trainIndex,]


model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + Deck, 
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
