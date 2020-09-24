# R workshop Day 2_03 

# libraries
library(AmesHousing)
library(rsample)
library(caret)

# access data
ames <- AmesHousing::make_ames()

# initial dimension
dim(ames)
## [1] 2930   81

# response variable
head(ames$Sale_Price)
## [1] 215000 105000 172000 244000 189900 195500

# data partitions

# Using base R
set.seed(42)  # for reproducibility
index_1 <- sample(1:nrow(ames), round(nrow(ames) * 0.8))
train_1 <- ames[index_1, ]
test_1  <- ames[-index_1, ]

# Using caret package
set.seed(42)  # for reproducibility
index_2 <- createDataPartition(ames$Sale_Price, p = 0.8, list = FALSE)
train_2 <- ames[index_2, ]
test_2  <- ames[-index_2, ]

library(tidyverse)
library(ggplot2)
library(reshape2)
library(rsample)
# have to use list in the next one because of different lengths of the variables, dataframe won't work
m1 <- list(train_1=train_1$Sale_Price,test_1=test_1$Sale_Price,train_2=train_2$Sale_Price, test_2=test_2$Sale_Price)
m1m<- melt(m1)
ggplot(m1m,aes(x=value,color=L1)) + geom_density(alpha=0.5)
ggplot(m1m,aes(x=value, fill=L1)) + geom_boxplot()

# stratified sampling
library(rsample)
index_3 <- initial_split(ames,0.8, strata = "Sale_Price", breaks=4)
train_3 <- training(index_3)
test_3  <- testing(index_3)
m2 <- list(train_3=train_3$Sale_Price,test_3=test_3$Sale_Price)
m2m<- melt(m2)
ggplot(m2m,aes(x=value,color=L1)) + geom_density(alpha=0.5)
ggplot(m2m,aes(x=value, fill=L1)) + geom_boxplot()


# saving copies of the final processed files to use in other projects
write.csv(train_3,file='train_3.csv',row.names=F)
write.csv(test_3,file='test_3.csv',row.names=F)


# target engineering and arranging multiple plots
library(gridExtra)
ggplot(train_3, aes(x=Sale_Price))+geom_histogram(color='white')
ggplot(train_3, aes(x=log(Sale_Price)))+geom_histogram(color='white')

# histogram plot comparison of regular and logged sales
plot_1<-ggplot(train_3, aes(x=Sale_Price))+geom_histogram(color='white')+labs(title='Sale Price')
plot_2<-ggplot(train_3, aes(x=log(Sale_Price)))+geom_histogram(color='white')+labs(title='Log Sale Price')
grid.arrange(plot_1,plot_2, ncol=1)

# normal probability quantile plots       
plot_1b <- ggplot(train_3, aes(sample = Sale_Price))+ stat_qq() + stat_qq_line()+labs(title='Sale Price')
plot_1b
plot_2b <- ggplot(train_3, aes(sample = log(Sale_Price)))+ stat_qq() + stat_qq_line()+labs(title='Log Sale Price')
plot_2b
grid.arrange(plot_1b,plot_2b, ncol=2)

shapiro.test(train_3$Sale_Price)
shapiro.test(log(train_3$Sale_Price))


# Box Cox transform
library(recipes) # allows pre-processing of variables prior to modeling
simple_trans_rec <- recipe(Sale_Price ~ ., data = train_3) %>%
  step_BoxCox(Sale_Price) %>%
  prep(training = train_3)

simple_trans_result <- bake(simple_trans_rec, train_3)

library(EnvStats) # Box Cox in EnvStats
box_1<-boxcox(train_3$Sale_Price, optimize=T)
print(box_1)

# Box Cox using the package forecast 
library(forecast)
box_2<-BoxCox.lambda(train_3$Sale_Price)
box_2
train_3t<-BoxCox(train_3$Sale_Price,lambda= "auto")
train_3t_df<-data.frame(train_3t)
colnames(train_3t_df)[1]<-"Box_Cox_Sale_Price"

plot_1<-ggplot(train_3, aes(x=Sale_Price))+
  geom_histogram(color='white')+labs(title='Sale Price')
plot_2<-ggplot(train_3, aes(x=log(Sale_Price)))+
  geom_histogram(color='white')+labs(title='Log Sale Price')
plot_3<-ggplot(train_3t_df, aes(x=Box_Cox_Sale_Price))+
  geom_histogram(color='white')+labs(title='Box Cox Sale Price')
grid.arrange(plot_1,plot_2,plot_3, ncol=1)
# InvBoxCox(x, lambda, biasadj = FALSE, fvar = NULL) will get  variable back

# are any values missing?
is.na(ames) %>% table()
sum(is.na(ames))

sum(is.na(AmesHousing::ames_raw)) # back to the preprocessed version

library(mice)
library(VIM)
library(AmesHousing)

mpat<-md.pattern(ames_raw)

aggr(ames_raw)

png(file="mdpat.png",width=1600,height=800)
aggr(ames_raw)
dev.off()
matrixplot(ames_raw)

mdcount<-sapply(ames_raw, function(x) sum(is.na(x)))
mdcount %>% data.frame(mdcount) %>% arrange(desc(mdcount))

library(visdat)
vis_miss(ames_raw)
vis_miss(ames_raw, cluster=T, sort_miss=T)
