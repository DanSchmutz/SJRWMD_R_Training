# glmnet regression
library(tidyverse)

# Helper packages
library(recipes)  # for feature engineering

# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process

# Model interpretability packages
library(vip)      # for variable importance


# Create training feature matrices
# we use model.matrix(...)[, -1] to discard the intercept
X <- model.matrix(Sale_Price ~ ., train_3)[, -1]
Xnotused<-model.matrix(Sale_Price ~.,train_3)

# transform y with log transformation
Y <- log(train_3$Sale_Price)

# Apply ridge regression to ames data
ridge <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

plot(ridge, xvar = "lambda")


str(ridge) # what does the object look like

# lambdas applied to penalty parameter
ridge$lambda %>% head()
## [1] 285.8055 260.4153 237.2807 216.2014 196.9946 179.4942

# small lambda results in large coefficients
coef(ridge)[c("Latitude", "Overall_QualVery_Excellent"), 100]
##                   Latitude Overall_QualVery_Excellent 
##                  0.4048216                  0.1423770

# large lambda results in small coefficients
coef(ridge)[c("Latitude", "Overall_QualVery_Excellent"), 1]  
##                                      Latitude 
## 0.0000000000000000000000000000000000063823847 
##                    Overall_QualVery_Excellent 
## 0.0000000000000000000000000000000000009838114

ridge$lambda[2]
ridge$lambda[99]

# Apply CV ridge regression to ames data
ridge <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 0
)
plot(ridge, main = "Ridge penalty\n\n")

# Ridge model
min(ridge$cvm)       # minimum MSE
## [1] 0.01968996
ridge$lambda.min     # lambda for this min MSE
## [1] 0.1417314

ridge$cvm[ridge$lambda == ridge$lambda.1se]  # 1-SE rule
## [1] 0.01975572
ridge$lambda.1se  # lambda for this MSE
## [1] 0.6279583

ridge$lambda.min
#[1] 0.1417314
log(ridge$lambda.min)
#[1] -1.953821
ridge$lambda.1se
#[1] 0.6279583
log(ridge$lambda.1se)
#[1] -0.4652815


# evaluation on train_3 ridge
predridge <- predict(ridge, X)
# compute RMSE of transformed predicted
RMSE(exp(predridge), exp(Y))

# predicting test_3 using ridge
Xtest <- model.matrix(Sale_Price ~ ., test_3)[, -1]
Ytest <- log(test_3$Sale_Price)
predridgetest <- predict(ridge, Xtest)
RMSE(exp(predridgetest), exp(Ytest))

test_3aug$predridgetest<-exp(predridgetest)
ggplot(test_3aug,aes(x=predridgetest,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')
cor(test_3aug$predridgetest,test_3aug$Sale_Price)^2
res_predridgetest<-test_3aug$Sale_Price-test_3aug$predridgetest
(mean((res_predridgetest)^2))^0.5


#lasso on ames data

lasso <- glmnet(
  x = X,
  y = Y,
  alpha = 1
)

plot(lasso, xvar = "lambda")


# Apply CV lasso regression to Ames data
lasso <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1
)
plot(lasso, main = "Lasso penalty\n\n")

# Lasso model
min(lasso$cvm)       # minimum MSE
# [1] 0.02046739
lasso$lambda.min     # lambda for this min MSE
# [1] 0.002782246
lasso$nzero[lasso$lambda == lasso$lambda.min] # No. of coef | Min MSE
#s50  137
lasso$cvm[lasso$lambda == lasso$lambda.1se]  # 1-SE rule
#[1] 0.02294067
lasso$lambda.1se  # lambda for this MSE
# [1] 0.01232708
lasso$nzero[lasso$lambda == lasso$lambda.1se] # No. of coef | 1-SE MSE
# s34 57


lasso$lambda.min
#[1] 0.002535079
log(lasso$lambda.min)
#[1] -5.977531
lasso$lambda.1se
#0.01352895
log(lasso$lambda.1se)
#-4.302923


# evaluation on train_3 lasso
predlasso <- predict(lasso, X)
# compute RMSE of transformed predicted
RMSE(exp(predlasso), exp(Y))

# predicting test_3 using lasso
Xtest <- model.matrix(Sale_Price ~ ., test_3)[, -1]
Ytest <- log(test_3$Sale_Price)
predlassotest <- predict(lasso, Xtest)
RMSE(exp(predlassotest), exp(Ytest))

test_3aug$predlassotest<-exp(predlassotest)
ggplot(test_3aug,aes(x=predlassotest,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')
cor(test_3aug$predlassotest,test_3aug$Sale_Price)^2
res_predlassotest<-test_3aug$Sale_Price-test_3aug$predlassotest
(mean((res_predlassotest)^2))^0.5


# elastic net model
# for reproducibility
set.seed(42)

# grid search across 
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet$bestTune
##   alpha     lambda
## 8   0.1 0.04728445

# results for model with lowest RMSE
cv_glmnet$results %>%
  filter(alpha == cv_glmnet$bestTune$alpha, lambda == cv_glmnet$bestTune$lambda)
##   alpha     lambda      RMSE  Rsquared        MAE    RMSESD RsquaredSD
## 1   0.1 0.04728445 0.1382785 0.8852799 0.08427956 0.0273119 0.04179193
## MAESD
## 1 0.005330001

# plot cross-validated RMSE
ggplot(cv_glmnet)

# evaluation on train_3 elastic net
predelastic <- predict(cv_glmnet, X)
# compute RMSE of transformed predicted
RMSE(exp(predelastic), exp(Y))

# predicting test_3 using elastic net
Xtest <- model.matrix(Sale_Price ~ ., test_3)[, -1]
Ytest <- log(test_3$Sale_Price)
predelastictest <- predict(cv_glmnet, Xtest)
RMSE(exp(predelastictest), exp(Ytest))

test_3aug$predelastictest<-exp(predelastictest)
ggplot(test_3aug,aes(x=predelastictest,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')
cor(test_3aug$predelastictest,test_3aug$Sale_Price)^2
res_predelastictest<-test_3aug$Sale_Price-test_3aug$predelastictest
(mean((res_predelastictest)^2))^0.5

# variable importance plot
vip(cv_glmnet, num_features = 20, geom = "point")

