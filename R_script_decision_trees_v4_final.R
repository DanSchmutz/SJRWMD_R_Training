# decision trees

library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects


# fitting using class appears appropriate here
rp_ttrain4cl <- rpart(
  formula = Survived2 ~ .,
  data    = ttrain4,
  method  = "class"
)

summary(rp_ttrain4cl)
rpart.plot(rp_ttrain4cl)
plotcp(rp_ttrain4cl)
rpart.rules(rp_ttrain4cl, extra=4)

# vip(rp_ttrain4cl, num_features = 40, bar = FALSE)
vip(rp_ttrain4cl)

# Construct partial dependence plots
p1 <- partial(rp_ttrain4cl, pred.var = "Gender") %>% autoplot()
p2 <- partial(rp_ttrain4cl, pred.var = "Fare") %>% autoplot()
p3 <- partial(rp_ttrain4cl, pred.var = "Pclass2") %>% autoplot()
p4 <- partial(rp_ttrain4cl, pred.var = "Age") %>% autoplot() 

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 4)

# two variable partial dependence plot 3D
p5 <- partial(rp_ttrain4cl, pred.var = c("Age", "Fare")) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, 
              colorkey = TRUE, screen = list(z = -20, x = -60))
p5

p6 <- partial(rp_ttrain4cl, pred.var = c("Age", "Fare"))
plotPartial(p6)

p7 <- partial(rp_ttrain4cl, pred.var = c("Gender", "Age"))
plotPartial(p7)

p8 <- partial(rp_ttrain4cl, pred.var = c("Age", "Fare", "Gender"))
plotPartial(p8)

# Construct partial dependence plots
p1 <- partial(rp_ttrain4cl, pred.var = "Gender") %>% autoplot()
p2 <- partial(rp_ttrain4cl, pred.var = "Fare") %>% autoplot()
p3 <- partial(rp_ttrain4cl, pred.var = c("Gender", "Fare")) %>% 
  plotPartial(levelplot = FALSE, zlab = "yhat", drape = TRUE, 
              colorkey = TRUE, screen = list(z = -20, x = -60))

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

rp_ttrain4clmany <- rpart(
  formula = Survived2 ~ .,
  data    = ttrain4,
  method  = "class",
  cp = 0.000001
)
rpart.plot(rp_ttrain4clmany)

# evaluating parsimonious and overfit models on train and test
pred_rp_ttrain4cl_train<-predict(rp_ttrain4cl, newdata=ttrain4)
pred_rp_ttrain4cl_test<-predict(rp_ttrain4cl, newdata=ttest4)
pred_rp_ttrain4clmany_train<-predict(rp_ttrain4clmany, newdata=ttrain4)
pred_rp_ttrain4clmany_test<-predict(rp_ttrain4clmany, newdata=ttest4)

hist(pred_rp_ttrain4cl_train) # base r histogram works here

# evaluating results
library(ROCit)
ROCit_obj <- rocit(score=pred_rp_ttrain4cl_train[,2],class=ttrain4$Survived2)
plot(ROCit_obj, YIndex=F, values=T)
ciAUC(ROCit_obj)

ROCit_obj <- rocit(score=pred_rp_ttrain4cl_test[,2],class=ttest4$Survived2)
plot(ROCit_obj, YIndex=F, values=T)
ciAUC(ROCit_obj)

ROCit_obj <- rocit(score=pred_rp_ttrain4clmany_train[,2],class=ttrain4$Survived2)
plot(ROCit_obj, YIndex=F, values=T)
ciAUC(ROCit_obj)

ROCit_obj <- rocit(score=pred_rp_ttrain4clmany_test[,2],class=ttest4$Survived2)
plot(ROCit_obj, YIndex=F, values=T)
ciAUC(ROCit_obj)

# confusion matrices

# confusion matrix for train parsimonious model
predclass<-pred_rp_ttrain4cl_train[,2] %>% 
  data.frame() %>% 
  mutate(predclass = ifelse(. > 0.5,1,0)) %>% 
  select(predclass)
  
confusionMatrix(factor(predclass$predclass),ttrain4$Survived2)

# confusion matrix for test parsimonious model
predclass<-pred_rp_ttrain4cl_test[,2] %>% 
  data.frame() %>% 
  mutate(predclass = ifelse(. > 0.5,1,0)) %>% 
  select(predclass)

confusionMatrix(factor(predclass$predclass),ttest4$Survived2)

# confusion matrix for train overfit model
predclass<-pred_rp_ttrain4clmany_train[,2] %>% 
  data.frame() %>% 
  mutate(predclass = ifelse(. > 0.5,1,0)) %>% 
  select(predclass)

confusionMatrix(factor(predclass$predclass),ttrain4$Survived2)

# confusion matrix for test overfit model
predclass<-pred_rp_ttrain4clmany_test[,2] %>% 
  data.frame() %>% 
  mutate(predclass = ifelse(. > 0.5,1,0)) %>% 
  select(predclass)

confusionMatrix(factor(predclass$predclass),ttest4$Survived2)
