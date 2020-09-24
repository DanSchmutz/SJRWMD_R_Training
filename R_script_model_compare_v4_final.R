# comparing multiple models using caret

# train_tknn 
# test_tknn

library(caret)
library(mda)
library(fastAdaboost)
library(plyr)
library(import)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(42)
model_rf <- train(Survived2~., data=train_tknn, method="rf", trControl=control, verbose=FALSE)
set.seed(42)
model_blr <- train(Survived2~., data=train_tknn, method="LogitBoost", trControl=control, verbose=FALSE)
set.seed(42)
model_fada <- train(Survived2~., data=train_tknn, method="adaboost", trControl=control, verbose=FALSE)

results_mc <- resamples(list(RF=model_rf, LBOOST=model_blr, ADA=model_fada))
summary(results_mc)
bwplot(results_mc)
dotplot(results_mc)

# compare predictions on test
pred_model_rf_test_tknn<-predict(model_rf,newdata=test_tknn)
pred_model_blr_test_tknn<-predict(model_blr,newdata=test_tknn)
pred_model_fada_test_tknn<-predict(model_fada,newdata=test_tknn)
confusionMatrix(pred_model_rf_test_tknn,test_tknn$Survived2)
confusionMatrix(pred_model_blr_test_tknn,test_tknn$Survived2)
confusionMatrix(pred_model_fada_test_tknn,test_tknn$Survived2)

