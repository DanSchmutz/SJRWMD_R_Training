## Logistic Regression and Titanic Dataset

# install.packages("titanic"), if needed
library(titanic)
library(tidyverse)
library(skimr)

skim(titanic_train)
library(visdat)
vis_miss(titanic_train)

ttrain<-titanic_train
ttrain[ttrain==""] <- NA
vis_miss(train)
View(ttrain)

# Exploratory Data Analysis

ttrain2<- ttrain %>% dplyr::select(-Cabin, -PassengerId, -Ticket)
# Dropping Cabin and PassengerID as not likely to help determine survival

class(ttrain2$Sex)

# making factors
ttrain2$Gender<-factor(ttrain2$Sex)
ttrain2$Pclass2 <- factor(ttrain2$Pclass, order=TRUE, levels = c(3, 2, 1))
ttrain2$Survived2 <- factor(ttrain2$Survived)
ttrain3<- ttrain2 %>% dplyr::select(-Survived, -Sex, -Pclass)

# EDA plots
library(ggpubr)
# Titanic Plot 1
plot_count <- ggplot(ttrain3, aes(x = Gender, fill = Survived2)) +
  geom_bar() +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  ggtitle("Most of the Titanic Passengers are Male.\n 
Most Passengers Who Survived Were Female") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

plot_percent <- ggplot(ttrain3, aes(x = Gender, fill = Survived2)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  ggtitle("75% of all Female Passengers Survived whereas only \n around 20% of the Male Passengers Survived") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggarrange(plot_count, plot_percent)

### Titanic Plot 2
plot_count <- ggplot(ttrain3, aes(x = Gender, fill = Survived2)) +
  geom_bar() +
  facet_wrap(~Pclass2) +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  theme(legend.position = "bottom")

plot_percent <- ggplot(ttrain3, aes(x = Gender, fill = Survived2)) +
  geom_bar(position = "fill") +
  facet_wrap(~Pclass2) +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  theme(legend.position = "bottom") +
  ylab("%")

combined_figure <- ggarrange(plot_count, plot_percent)
annotate_figure(combined_figure,
                top = text_grob("Almost All Female Passengers Who are Class One and Two Survived. The Big Proportion of Men not Surviving \n Mainly Comes From Male Class 3 Passengers",
                                color = "black",
                                face = "bold",
                                size = 14
                )
)


### Titanic Plot 3
plot_count <- ggplot(titanic[1:891, ], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  ggtitle("Most of the Titanic Passengers are Male.\n 
Most Passengers Who Survived Were Female") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

plot_percent <- ggplot(titanic[1:891, ], aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  ggtitle("75% of all Female Passengers Survived whereas only \n around 20% of the Male Passengers Survived") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggarrange(plot_count, plot_percent)

plot_age <- ggplot(ttrain3, aes(x = Age, fill = Survived2)) +
  geom_histogram() +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  theme(legend.position = "bottom")

plot_fare <- ggplot(ttrain3, aes(x = Fare, fill = Survived2)) +
  geom_histogram() +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  theme(legend.position = "bottom")

plot_embarked <- ggplot(ttrain3, aes(x = Embarked, fill = Survived2)) +
  geom_bar() +
  scale_fill_manual(
    name = "Survived",
    values = c("red", "blue"),
    labels = c("No", "Yes"),
    breaks = c("0", "1")
  ) +
  theme(legend.position = "bottom")

ggarrange(plot_age, 
          plot_fare, 
          plot_embarked, common.legend = TRUE, ncol = 3)


# continuing to engineer train data
median(ttrain3$Age, na.rm=T)
ttrain4<- ttrain3 %>% dplyr::select(-Name)
ttrain4$Age[is.na(ttrain4$Age)]<-28
vis_miss(ttrain4)
sum(is.na(ttrain4))
table(ttrain4$Embarked)
ttrain4$Embarked[is.na(ttrain4$Embarked)]<-"S"
vis_miss(ttrain4)


# applying same steps to test
ttest<-titanic_test
ttest[ttest==""] <- NA
ttest2<- ttest %>% dplyr::select(-Cabin, -PassengerId, -Ticket)
ttest2$Gender<-factor(ttest2$Sex)
ttest2$Pclass2 <- factor(ttest2$Pclass, order=TRUE, levels = c(3, 2, 1))
# finding the actual test labels
titanic3 <- read_csv("titanic3.csv")
titanicjoinfile<- titanic3 %>% select(name, survived)
ttest2b<-left_join(ttest2,titanicjoinfile,by=c('Name'='name'))
ttest2b$Survived<-ttest2b$survived
ttest2b$Survived2 <- factor(ttest2b$Survived)
ttest3<- ttest2b %>% dplyr::select(-Survived, -survived, -Sex, -Pclass)
ttest4<- ttest3 %>% dplyr::select(-Name)
ttest4$Age[is.na(ttest4$Age)]<-28
ttest4$Embarked[is.na(ttest4$Embarked)]<-"S"
vis_miss(ttest4)
ttest4<- ttest4 %>% drop_na()
vis_miss(ttest4)

# saving copies of the final processed files
write.csv(ttrain4,file='ttrain4.csv',row.names=F)
write.csv(ttest4,file='ttest4.csv',row.names=F)

# fitting one variable logistic regression
logis1<-glm(Survived2 ~ ., family = binomial(link='logit'), data = ttrain4)
summary(logis1)
plot(logis1)
library(caret)
ggplot(data.frame(logis1$fitted.values),aes(x=logis1.fitted.values))+geom_histogram(col='white')

install.packages('ROCR')
library(ROCR)
install.packages('ROCit')
library(ROCit)
ROCit_obj <- rocit(score=logis1$fitted.values,class=ttrain4$Survived2)
plot(ROCit_obj, YIndex=F, values=T)
ciAUC(ROCit_obj)

predg1 <- prediction(logis1$fitted.values,ttrain4$Survived2)
roc.perf1 = performance(predg1, measure = "tpr", x.measure = "fpr")
plot(roc.perf1)
abline(a=0,b=1)
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf1, predg1))

# sensitivity 0.7719298
# specificity 0.8051002
# cutoff      0.3785297

# let's evaluate confusion matrix using the traditional >0.5 cut off and the 0.3785297 for both train and test

pred_ttrain4_df<-ttrain4
pred_ttrain4_df$logis1<-logis1$fitted.values
pred_ttrain4_df2<- pred_ttrain4_df %>% mutate(cutpt5= ifelse(logis1 > 0.5,1,0), cutpt38 = ifelse(logis1 > 0.3785297,1,0))
str(pred_ttrain4_df2)
table(pred_ttrain4_df2$cutpt5,pred_ttrain4_df2$cutpt38) # not the same

confusionMatrix(factor(pred_ttrain4_df2$cutpt5),pred_ttrain4_df2$Survived2)
confusionMatrix(factor(pred_ttrain4_df2$cutpt38),pred_ttrain4_df2$Survived2)

# viewing the partial residuals to understand relative effects of the variables
library(visreg)
visreg(logis1)

# predictions on the test set
pred_ttest4<-predict(logis1,newdata=ttest4,type="response") # must use response to get the probabilities
pred_ttest4_df<-ttest4
pred_ttest4_df$logis1<-pred_ttest4
pred_ttest4_df2<- pred_ttest4_df %>% mutate(cutpt5= ifelse(logis1 > 0.5,1,0), cutpt38 = ifelse(logis1 > 0.3785297,1,0)) 
str(pred_ttest4_df2)

confusionMatrix(factor(pred_ttest4_df2$cutpt5),pred_ttest4_df2$Survived2)
confusionMatrix(factor(pred_ttest4_df2$cutpt38),pred_ttest4_df2$Survived2)




