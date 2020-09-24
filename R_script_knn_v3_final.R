# knn on the Titanic dataset

# Helper packages
library(tidyverse)
library(dplyr)      # for data wrangling
library(ggplot2)    # for awesome graphics
library(rsample)    # for creating validation splits
library(recipes)    # for feature engineering

# Modeling packages
library(caret)       # for fitting KNN models

# extra processing to avoid error
ttrain4knn<-ttrain4  %>% 
  mutate(Survived2 = factor(Survived2, 
                        labels = make.names(levels(Survived2))))

# Create blueprint
blueprint <- recipe(Survived2 ~ ., data = ttrain4knn) %>%
  step_nzv(all_nominal()) %>% # remove sparse variables
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

# Create a resampling method
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  classProbs = TRUE,                 
  summaryFunction = twoClassSummary
)

# Create a hyperparameter grid search
hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(ttrain4)/3, length.out = 20))
)

# creating a tighter grid
hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(ttrain4)/20, length.out = 20))
)

# Fit knn model and perform grid search
knn_grid <- train(
  blueprint, 
  data = ttrain4knn, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "ROC"
)

ggplot(knn_grid)
print(knn_grid)

# baking to make prepared train and test
trained_rec <- prep(blueprint, training = ttrain4)
train_tknn <- bake(trained_rec, new_data = ttrain4)
test_tknn  <- bake(trained_rec, new_data = ttest4)

library(class)
train_tknn_mod<-knn(train = train_tknn, test = train_tknn,cl = train_tknn$Survived2, k=7)
confusionMatrix(train_tknn_mod,train_tknn$Survived2)
test_tknn_mod<-knn(train = train_tknn, test = test_tknn,cl = train_tknn$Survived2, k=7)


