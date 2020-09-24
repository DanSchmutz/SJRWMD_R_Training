### random forest

library(randomForest)
library(caret)

# extra processing to avoid error
ttrain4rf<-ttrain4  %>% 
  mutate(Survived2 = factor(Survived2, 
                            labels = make.names(levels(Survived2))))

ttest4rf<-ttest4  %>% 
  mutate(Survived2 = factor(Survived2, 
                            labels = make.names(levels(Survived2))))


# code below used to identify optimal mtry hyperparameter for tuning using caret
seed<-42
set.seed(seed)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid", classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(1:7))
metric<-"Accuracy"
rf_gridsearch <- train(Survived2~., data= ttrain4rf, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
print(rf_gridsearch)


rfm_ttrain4_500<-randomForest(Survived2~.,ttrain4rf,mtry=3,ntree=500) # perform random forest using 80% training dataset and optimized hyperparameters

print(rfm_ttrain4_500) # view OOB estimate of error rate
plot(rfm_ttrain4_500)

rfm_ttrain4_10000<-randomForest(Survived2~.,ttrain4rf,mtry=3,ntree=10000) # perform random forest using 80% training dataset and optimized hyperparameters

print(rfm_ttrain4_10000) # view OOB estimate of error rate, no improvement over 500 trees
plot(rfm_ttrain4_10000)


pred_rfm_500_test<-predict(rfm_ttrain4_500,newdata=ttest4rf) # make predictions on 20% test dataset using training dataset-developed model
confusionMatrix(pred_rfm_500_test,ttest4rf$Survived2) # evaluating performance on out-of-sample test dataset 

pred_rfm_10000_test<-predict(rfm_ttrain4_10000,newdata=ttest4rf) 
confusionMatrix(pred_rfm_10000_test,ttest4rf$Survived2) 

rfm_ttrain4_500i<-randomForest(Survived2~.,ttrain4rf,mtry=3,ntree=500, importance=T)

varImpPlot(rfm_ttrain4_500i) # visualize variable importance

# Construct partial dependence plots
p11 <- partial(rfm_ttrain4_500i, pred.var = "Gender") %>% autoplot()
p12 <- partial(rfm_ttrain4_500i, pred.var = "Pclass2") %>% autoplot()
p13 <- partial(rfm_ttrain4_500i, pred.var = "Fare") %>% autoplot()
p14 <- partial(rfm_ttrain4_500i, pred.var = "Age") %>% autoplot() 

# Display plots side by side
gridExtra::grid.arrange(p11, p12, p13, p14, ncol = 4)

# random forest on ames data
m_rf_ames_v1<-randomForest(Sale_Price~., data=train_3)
print(m_rf_ames_v1)
plot(m_rf_ames_v1)

# warning code below took more than 30 min so terminated process
# code below used to identify optimal mtry hyperparameter for tuning using caret
seed<-42
set.seed(seed)
control <- trainControl(method="cv", number=10, search="grid")
tunegrid <- expand.grid(.mtry=c(1:80))
metric<-"RMSE"
rf_gridsearch <- train(Sale_Price~., data= train_3, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
print(rf_gridsearch)

m_rf_ames_v1<-randomForest(Sale_Price~., data=train_3)
print(m_rf_ames_v1)
plot(m_rf_ames_v1)

pred_rf_ames_v1<-predict(m_rf_ames_v1,newdata=test_3)

RMSE(m_rf_ames_v1$predicted, train_3$Sale_Price)
RMSE(pred_rf_ames_v1, test_3$Sale_Price)

test_3aug$predrf1<-pred_rf_ames_v1
ggplot(test_3aug,aes(x=predrf1,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')
cor(test_3aug$predrf1,test_3aug$Sale_Price)^2
res_predrf1<-test_3aug$Sale_Price-test_3aug$predrf1
(mean((res_predrf1)^2))^0.5
