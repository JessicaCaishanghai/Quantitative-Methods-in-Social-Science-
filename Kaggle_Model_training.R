
library(readr)
train1 <- read_csv('training.csv')
test <- read_csv('testing.csv')
train <- train1
View(train)
train <- read.csv('training.csv', fileEncoding = "encoding type")
any(is.na(train$outcome))
# Process the train$cups data
train$cups <- ifelse(train$cups == "More than 4", 5, train$cups)
train$cups <- ifelse(train$cups == "Less than 1", 0, train$cups)
train$cups <- as.numeric(train$cups)

#Process the age factors 
library(dplyr)

train$age <- recode(train$age, 
                     "55-64 years old" = 60,
                     "25-34 years old" = 30,
                     "35-44 years old" = 40,
                     "18-24 years old" = 21,
                     "45-54 years old" = 50,
                     ">65 years old" = 70,
                     "<18 years old" = 10)
summary(train$age)

#Process Favorite drink and prefer_kind
any(is.na(train$favorite_drink))
train$favorite_drink[is.na(train$favorite_drink)] <- "unknown"
train$favorite_drink <- as.factor(train$favorite_drink)
train$prefer_kind <- as.factor(train$prefer_kind)
unique(train$prefer_strength)

#prefer_strength_new
train$prefer_strength_new[is.na(train$prefer_strength)] <- 3
train$prefer_strength_new <- recode(train$prefer_strength, 
                    "Weak" = 1,
                    "Somewhat light" = 2,
                    "Medium" = 3,
                    "Somewhat strong" = 4,
                    "Very strong" = 5)

train$prefer_strength <- train$prefer_strength_new
#t_c
train$prefer_roast[is.na(train$prefer_strength)] <- 'Medium'
train$prefer_roast_c <- as.factor(train$prefer_roast)

#prefer_caffine
train$prefer_caffeine[is.na(train$prefer_caffeine)] <- 'Half caff'
train$prefer_caffeine <- as.factor((train$prefer_caffeine))
class(train$a_bitterness)

library(MASS)

#same for test process
test$cups <- ifelse(test$cups == "More than 4", 5, test$cups)
test$cups <- ifelse(test$cups == "Less than 1", 0, test$cups)
test$cups <- as.numeric(test$cups)

# Process the 'age' factors
library(dplyr)

test$age <- recode(test$age, 
                   "55-64 years old" = 60,
                   "25-34 years old" = 30,
                   "35-44 years old" = 40,
                   "18-24 years old" = 21,
                   "45-54 years old" = 50,
                   ">65 years old" = 70,
                   "<18 years old" = 10)

# Process 'favorite_drink' and 'prefer_kind'
any(is.na(test$favorite_drink))
test$favorite_drink[is.na(test$favorite_drink)] <- "unknown"
test$favorite_drink <- as.factor(test$favorite_drink)
test$prefer_kind <- as.factor(test$prefer_kind)

# Process 'prefer_strength' including creating a new variable 'prefer_strength_new'
test$prefer_strength_new <- ifelse(is.na(test$prefer_strength), 3, test$prefer_strength)
test$prefer_strength_new <- recode(test$prefer_strength_new, 
                                   "Weak" = 1,
                                   "Somewhat light" = 2,
                                   "Medium" = 3,
                                   "Somewhat strong" = 4,
                                   "Very strong" = 5)

test$prefer_strength <- test$prefer_strength_new

# Process 'prefer_roast', including handling NAs and creating a new categorical variable 'prefer_roast_c'
test$prefer_roast[is.na(test$prefer_roast)] <- 'Medium'
test$prefer_roast_c <- as.factor(test$prefer_roast)

# Process 'prefer_caffeine', including handling NAs
test$prefer_caffeine[is.na(test$prefer_caffeine)] <- 'Half caff'
test$prefer_caffeine <- as.factor(test$prefer_caffeine)

# Regression model 
model <- polr( outcome ~ age + favorite_drink + prefer_kind + prefer_strength+prefer_roast_c+prefer_caffeine
             +self_experience+a_bitterness+a_acidity+a_preference + b_bitterness+b_acidity+b_preference
             +c_bitterness+c_acidity+c_preference+d_bitterness+d_acidity,data=train)


test$cups <- as.numeric(test$cups)
selected_features_test <- test[, c("age",'favorite_drink','prefer_kind','prefer_strength',
                                   'prefer_roast_c','prefer_caffeine',
                                   'self_experience','a_bitterness','a_acidity','a_preference','b_bitterness','b_acidity','b_preference',
                                   'c_bitterness','c_acidity','c_preference','d_bitterness','d_acidity')]
test$predictions <-  predict(model, newdata = selected_features_test)
model1 <- polr(outcome ~age+favorite_drink+prefer_kind+prefer_roast_c+prefer_caffeine+self_experience+a_bitterness,data = train,Hess= TRUE)
selected = test[,c('age','favorite_drink','prefer_kind','prefer_roast_c','prefer_caffeine','self_experience','a_bitterness')]
test$predictions <- predict(model1, newdata = selected)

train$outcome <- as.numeric(train$outcome) 

model_test <- lm(outcome ~ age+favorite_drink+prefer_kind+prefer_strength+prefer_roast_c+prefer_caffeine
                 +self_experience+a_bitterness+a_acidity+a_preference + b_bitterness+b_acidity+b_preference
                 +c_bitterness+c_acidity+c_preference+d_bitterness+d_acidity,data = train)
selected = test[,c("age",'favorite_drink','prefer_kind','prefer_strength',
                   'prefer_roast_c','prefer_caffeine',
                   'self_experience','a_bitterness','a_acidity','a_preference','b_bitterness','b_acidity','b_preference',
                   'c_bitterness','c_acidity','c_preference','d_bitterness','d_acidity')]


#Let's use mean to clean some data
mean_value <- mean(test$b_acidity, na.rm = TRUE)
test$b_acidity[is.na(test$b_acidity)] <- mean_value

# 用这个平均值替换该列中的NA值
data$variable[is.na(data$variable)] <- mean_value
test$predictions <- predict(model_test, newdata = selected)
round_predictions_test_datase <- round(test$predictions)
round_predictions_test_datase
summary(model_test) # I can find the most influential variables
# The most influential variables are:  age + favorite_drink + prefer_kind + a_acidity+ self_experience+ prefer_roast_c + prefer_caffeine + b_preference + c_bitterness + c_preference + d_bitterness


#to let na be the mean values of all the non na values
test$a_acidity[is.na(test$a_acidity)] <- mean(test$a_acidity,na.rm = TRUE)
test$c_bitterness[is.na(test$c_bitterness)] <- mean(test$c_bitterness,na.rm = TRUE)
test$c_preference[is.na(test$c_preference)] <- mean(test$c_preference,na.rm = TRUE)
test$d_bitterness[is.na(test$d_bitterness)] <- mean(test$d_bitterness, na.rm = TRUE)
test$b_preference[is.na(test$b_preference)] <- mean(test$b_preference, na.rm = TRUE)
#Eventual is my final model 
eventual <- lm( outcome ~ age + favorite_drink + prefer_kind + a_acidity+ self_experience+ prefer_roast_c + prefer_caffeine + b_preference + c_bitterness + c_preference + d_bitterness, data=train)
selected = test[,c("age",'favorite_drink','prefer_kind',
                   'prefer_roast_c','prefer_caffeine',
                   'self_experience','a_acidity','a_preference','b_bitterness','b_acidity','b_preference',
                   'c_bitterness','c_acidity','c_preference','d_bitterness','d_acidity')]

test$predictions <- predict(eventual, newdata = selected)
round_predictions <- round(test$predictions)

test$predictions <- round_predictions
test$outcome <- test$predictions
selected_data <- test[c("submission_id", "outcome")]

write.csv(selected_data, 'kaggle.csv', row.names = FALSE)

#Another trial 
summary(lm(outcome~prefer_caffeine+prefer_roast_c ,data=train))
class(train$trump_votes)
summary(lm(outcome~prefer_caffeine+prefer_roast_c+trump_votes,data = train))
model_test <- lm(outcome ~ age+favorite_drink+prefer_kind+prefer_strength+prefer_roast_c+prefer_caffeine
                 +self_experience+a_bitterness+a_acidity+a_preference + b_bitterness+b_acidity+b_preference
                 +c_bitterness+c_acidity+c_preference+d_bitterness+d_acidity,data = train)
summary(model_test
        )
