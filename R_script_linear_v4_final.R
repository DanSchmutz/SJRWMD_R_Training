# working on linear models with ames

# Helper packages
library(tidyverse)
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for crossvalidation, etc.

# Model interpretability packages
library(vip)      # variable importance
library(visreg) # visualizing partial residual plots

# basic linear regression
lm1 <- lm(Sale_Price ~ Gr_Liv_Area, data = train_3)
summary(lm1)

ggplot(train_3, aes(x= Gr_Liv_Area, y=Sale_Price))+
  geom_point()+stat_smooth(method="lm", se=F)

# saving training set prediction and residuals for plotting
t3a<-train_3
t3a$predicted<-predict(lm1)
t3a$resid<-residuals(lm1)
ggplot(t3a, aes(x= Gr_Liv_Area, y=Sale_Price))+
  geom_point()+
  stat_smooth(method="lm", se=F)+
  geom_point(aes(y = predicted), shape = 1)+
  geom_segment(aes(xend = Gr_Liv_Area, yend = predicted), alph=0.2)


sigma(lm1)    # RMSE same as residual standard error in output (with rounding)
## [1] 53992.84
sigma(lm1)^2  # MSE
## [1] 2915226792

confint(lm1, level = 0.95)
##             2.5 %     97.5 %
##(Intercept)  794.7637 14583.3121
## Gr_Liv_Area 111.0540   119.7955

# mutliple linear regression, two variables
lm2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = train_3)
summary(lm2)
coef(lm2)
sigma(lm2)

visreg2d(lm2, "Gr_Liv_Area", "Year_Built")

# mutliple linear regression, allowing interaction
lm2b <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built + Gr_Liv_Area:Year_Built, data = train_3)
summary(lm2b)
coef(lm2b)
sigma(lm2b)
visreg2d(lm2b, "Gr_Liv_Area", "Year_Built")

lm3 <- lm(Sale_Price ~ ., data = train_3)
sigma(lm3)
summary(lm3)
visreg(lm3) # let's visualize a few of the variable partial residuals

# using caret's cv to compare models using 10-fold cv
# Train model using 10-fold cross-validation
# model 1 CV
set.seed(42)  # for reproducibility
cv_model1 <- train(
  form = Sale_Price ~ Gr_Liv_Area, 
  data = train_3, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
# model 2 CV
set.seed(42)
cv_model2 <- train(
  Sale_Price ~ Gr_Liv_Area + Year_Built, 
  data =train_3, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# model 3 CV
set.seed(42)
cv_model3 <- train(
  Sale_Price ~ ., 
  data = train_3, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# Extract out of sample performance measures
sum123<-summary(resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3
)))
sum123

#better

results123<-resamples(list(
  model1 = cv_model1, 
  model2 = cv_model2, 
  model3 = cv_model3
))

summary(results123)
bwplot(results123)
dotplot(results123)


# adding residuals to examine assumptions

cv_model1$finalModel
cv_model3$finalModel

dflm1 <- broom::augment(cv_model1$finalModel, data = train_3)
str(dflm1)
glimpse(dflm1)

dflm3 <- broom::augment(cv_model3$finalModel, data = train_3)

p1lm1 <- ggplot(dflm1, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Sale_Price ~ Gr_Liv_Area")
p1lm1

p1lm3 <- ggplot(dflm3, aes(.fitted, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Predicted values") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Sale_Price ~ .")
p1lm3

grid.arrange(p1lm1,p1lm3,ncol=2)

# are the residuals correlated by row number?
dflm1 <- mutate(dflm1, id = row_number())
dflm3 <- mutate(dflm3, id = row_number())

p1 <- ggplot(dflm1, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 1", subtitle = "Correlated residuals.")+
  stat_smooth(method='loess',span=0.1)

p2 <- ggplot(dflm3, aes(id, .std.resid)) + 
  geom_point(size = 1, alpha = .4) +
  xlab("Row ID") +
  ylab("Residuals") +
  ggtitle("Model 3", subtitle = "Uncorrelated residuals.")+
  stat_smooth(method='loess',span=0.1)

gridExtra::grid.arrange(p1, p2, nrow = 1)

library(corrplot)
# look at correlations among the 35 numeric variables
train_3 %>% dplyr::select_if(is.numeric) %>% 
  cor() %>% corrplot()

# how do we do on prediction with the 3 models, evaluated on the test data?
pred_lm1<-predict(lm1,newdata=test_3)
pred_lm2<-predict(lm2,newdata=test_3)
pred_lm3<-predict(lm3,newdata=test_3) # problem with new level in roof material showing up in test set

test_3aug<-test_3
test_3aug$pred_lm1<-pred_lm1
test_3aug$pred_lm2<-pred_lm2

p70<-ggplot(test_3aug,aes(x=pred_lm1,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+
  geom_abline(slope=1, intercept=0, col='red')
p71<-ggplot(test_3aug,aes(x=pred_lm2,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+
  geom_abline(slope=1, intercept=0, col='red')
gridExtra::grid.arrange(p70, p71, nrow = 1)

cor(test_3aug$pred_lm1,test_3aug$Sale_Price)^2
cor(test_3aug$pred_lm2,test_3aug$Sale_Price)^2

res_lm1<-test_3aug$Sale_Price-test_3aug$pred_lm1
res_lm2<-test_3aug$Sale_Price-test_3aug$pred_lm2

(mean((res_lm1)^2))^0.5
(mean((res_lm2)^2))^0.5

# introducing glmulti, it has too many predictors (30 is max)
library(glmulti)
glm1<-glmulti(Sale_Price~.,data=train_3,crit="bic",level=1,method="d")
glm2<-glmulti(Sale_Price~ .,data=train_3,method="g",crit="bic",level=1,popsize=2,mutrate=0.05,sexrate=0.7,imm=0.2,deltaM=0.5,deltaB=0.1,conseq=6)

#trying again with smaller dataset
train_3num<-train_3 %>% dplyr::select_if(is.numeric)
glimpse(train_3num)
glm1<-glmulti(Sale_Price~.,data=train_3num,crit="bic",level=1,method="d")

train_3numcor<-corrr::correlate(train_3num) %>% select(rowname,Sale_Price) %>% 
  arrange(Sale_Price)
head(train_3numcor)
print(train_3numcor,n=nrow(train_3numcor))

train_3num_sm<-train_3num %>% select(-Misc_Val,-Year_Sold,-BsmtFin_SF_2,-Three_season_porch,-Mo_Sold)
glm1<-glmulti(Sale_Price~.,data=train_3num_sm,crit="bic",level=1,method="d")
glm2<-glmulti(Sale_Price~ .,data=train_3num_sm,method="g",crit="bic",level=1,popsize=5,mutrate=0.05,sexrate=0.7,imm=0.2,deltaM=0.5,deltaB=0.1,conseq=6) # this worked
glm3<-glmulti(Sale_Price~ .,data=train_3num_sm,method="g",crit="bic",level=1,popsize=100,mutrate=0.05,sexrate=0.7,imm=0.2,deltaM=0.5,deltaB=0.1,conseq=6)
glm4<-glmulti(Sale_Price~ .,data=train_3num_sm,method="g",crit="bic",level=1,popsize=1000,mutrate=0.05,sexrate=0.7,imm=0.2,deltaM=0.5,deltaB=0.1,conseq=6)

set.seed(42)  # for reproducibility
cv_modelglm4 <- train(
  form = Sale_Price ~ Lot_Frontage+Lot_Area+Year_Built+Year_Remod_Add+Mas_Vnr_Area+Bsmt_Unf_SF+Total_Bsmt_SF+First_Flr_SF+Second_Flr_SF+Bsmt_Full_Bath+Bedroom_AbvGr+Kitchen_AbvGr+TotRms_AbvGrd+Fireplaces+Garage_Cars+Wood_Deck_SF+Pool_Area+Latitude, 
  data = train_3num_sm, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

pred_lmglm4<-predict(cv_modelglm4$finalModel,newdata=test_3)


test_3aug$pred_lmglm4<-pred_lmglm4
ggplot(test_3aug,aes(x=pred_lmglm4,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')
cor(test_3aug$pred_lmglm4,test_3aug$Sale_Price)^2
res_lmglm4<-test_3aug$Sale_Price-test_3aug$pred_lmglm4
(mean((res_lmglm4)^2))^0.5


summary(glm4)
print(glm4)
glm4@formulas[1:6]
tmp<-weightable(glm4)
tmp <- tmp[tmp$bic <= min(tmp$bic) + 2,]
tmp
plot(glm4, type="r")
plot(glm4, type="s")

pred_lmglm46best<-predict(glm4,select=6,newdata=test_3)

test_3aug$pred_lmglm46best<-as.vector(pred_lmglm46best$averages)
ggplot(test_3aug,aes(x=pred_lmglm46best,y=Sale_Price))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')
cor(test_3aug$pred_lmglm46best,test_3aug$Sale_Price)^2
res_lmglm46best<-test_3aug$Sale_Price-test_3aug$pred_lmglm46best
(mean((res_lmglm46best)^2))^0.5

ggplot(test_3aug,aes(x=pred_lmglm46best,y=pred_lmglm4))+
  geom_point()+stat_smooth(method=lm)+geom_abline(slope=1, intercept=0, col='red')

ggplot(test_3aug,aes(x=(pred_lmglm46best-pred_lmglm4)))+geom_histogram(col='white')
