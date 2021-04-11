# Libraries Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)
library(rAverage)
library(tidyverse)
library(scales)
library(lubridate)
library(ggridges)

# energy applying muliple ridge lasso and elasticnet
library(readr)
energydata_complete <- read_csv("D:/data mininig/dataset/10000 rows and 10 columns/energy data/energydata_complete.csv")
data <- energydata_complete
head(data)
str(data)
# checking for redudancy in data
unique(data)
sapply(data,function(x) sum(is.na(x)))
# checking outliners and removing them
outlier <- round(1.5 * IQR(data$Appliances),0)
data %>% 
  mutate(outlier = ifelse(Appliances > outlier, "Outlier", "Not Outlier")) %>% 
  ggplot(aes(x=Appliances)) +
  geom_histogram(alpha = 0.5, fill = "#5EB296", colour = "#4D4D4D") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  ggtitle("Appliances VARIABLE IS  Normal now", subtitle = "Appliances not skewed for non-outliers") +
  labs(x= "Appliances", y= "Count") +
  facet_wrap(~ outlier, scales = "free")
# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

# Linear Model
set.seed(1234)
lm <- train(Appliances~.,train,method="lm",tr=custom)


# Results
par(mfrow=c(2,2))
plot(lm$finalModel)

lm$results





# Ridge Regression
set.seed(1234)
ridge <- train(Appliances~.,train,method="glmnet",tuneGrid=expand.grid(alpha=0, lambda=seq(0.0001,1,length=5)),trControl=custom)

# Plot Results
plot(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))
ridge$results

# Lasso Regression
set.seed(1234)
lasso <- train(Appliances~.,train,method="glmnet",tuneGrid=expand.grid(alpha=1, lambda=seq(0.0001,1,length=5)),trControl=custom)

# Plot Results
plot(lasso)
plot(lasso$finalModel, xvar = 'lambda', label=T)
lasso$results

# Elastic Net Regression
set.seed(1234)
en <- train(Appliances~.,train,method="glmnet",tuneGrid=expand.grid(alpha=seq(0,1,length(10)), lambda=seq(0.0001,1,length=5)),trControl=custom)

# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))
en$results
# applying KNN
set.seed(1234)
knn_model<- train(Appliances~.,train,method="knn", tunelength=115, trControl= custom, preProcess=c("center","scale"))
knn_model$results
plot(knn_model)
# Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)


# Prediction
p1 <- predict(knn_model, train)
p1

RMSE(p1,train$Appliances)
p2 <- predict(knn_model, test)

RMSE(p2,test$Appliances)
