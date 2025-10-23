
summary(train)
      
#PassengerId       Survived          Pclass          Name               Sex                 Age            SibSp           Parch       
#Min.   :  1.0   Min.   :0.0000   Min.   :1.000   Length:891         Length:891         Min.   : 0.42   Min.   :0.000   Min.   :0.0000  
#1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000   Class :character   Class :character   1st Qu.:20.12   1st Qu.:0.000   1st Qu.:0.0000  
#Median :446.0   Median :0.0000   Median :3.000   Mode  :character   Mode  :character   Median :28.00   Median :0.000   Median :0.0000  
#Mean   :446.0   Mean   :0.3838   Mean   :2.309                                         Mean   :29.70   Mean   :0.523   Mean   :0.3816  
#3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000                                         3rd Qu.:38.00   3rd Qu.:1.000   3rd Qu.:0.0000  
#Max.   :891.0   Max.   :1.0000   Max.   :3.000                                         Max.   :80.00   Max.   :8.000   Max.   :6.0000  
#                                                                                       NA's   :177                                     
#    Ticket               Fare           Cabin             Embarked        
# Length:891         Min.   :  0.00   Length:891         Length:891        
# Class :character   1st Qu.:  7.91   Class :character   Class :character  
# Mode  :character   Median : 14.45   Mode  :character   Mode  :character  
#                    Mean   : 32.20                                        
#                    3rd Qu.: 31.00                                        
#                    Max.   :512.33  

# NA - Age 
library(dplyr)
library(stringr)
library(tidyr)
titles <- train$Name %>% sub(".*?, (.*?)\\..*", "\\1", .) # extracting titles like Mr Sir etc.
title_list <- unique(titles)
#[1] "Mr"           "Mrs"          "Miss"         "Master"      
#[5] "Don"          "Rev"          "Dr"           "Mme"         
#[9] "Ms"           "Major"        "Lady"         "Sir"         
#[13] "Mlle"         "Col"          "Capt"         "the Countess"
#[17] "Jonkheer"

train$Title <- titles
data_1 <- train %>%
  mutate(
    Title = case_when(
      Title %in% c("Don", "Major", "Capt", "Jonkheer", "Rev", "Col") ~ "Mr",
      Title %in% c("Countess", "Mme") ~ "Mrs",
      Title %in% c("Mlle", "Ms") ~ "Miss",
      Title == "Dr" & Sex == "Male" ~ "Mr",
      Title == "Dr" & Sex == "Female" ~ "Mrs",
      TRUE ~ Title
    )
  )

# we have a lot of NA in Cabin column, so we might drop it
data_2$Deck <- data_1$Cabin 
data_2$Deck <- data_2$Deck %>% gsub("[^A-Z]", "", .) # but first we introduce Deck
data_2$Deck <- replace_na(data_2$Deck, "Unknown")
data_2 <- data_1 %>% select(-Cabin)
# passenger id does not also give any valuable information
data_2 <- data_2 %>% select(-PassengerId)
# we also do not really need ticket number (unless we decide to investigate sum supernatural effect of ticket numbers)
data_2 <- data_2 %>% select(-Ticket)
# embarkation also useless
data_2 <- data_2 %>% select(-Embarked)
# we also might wanna drop names for the sake of ml
data_2  <- data_2 %>% select(-Name)

# let's summarize the family size
data_2$FamilySize <- data_2$SibSp + data_2$Parch

# replace NA in age by a mean of ages
mean_ages <- mean(data_2$Age, na.rm = TRUE)
data_2$Age[is.na(data_2$Age)] <- mean_ages
