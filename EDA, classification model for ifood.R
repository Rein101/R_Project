## Setting up the Environment
library(plyr)
library(readr)
library(dplyr)
library(tidyverse)
library(caret)

## Data importation and cleaning
customer_market_data <- read_excel("C:/Users/MR MIGWI/Desktop/WORK/PORTFOLIO/Diagnostic Analytics/customer_market_data.xlsx")
View(customer_market_data)

# Removing rows with empty entries in the income column
customer_market_data <- na.omit(customer_market_data)

# missing values
sapply(customer_market_data, function(x) sum(is.na(x)))
# summary statistics
summary(customer_market_data)

## Visualizing Data for customer characeristic

ed<-ggplot(data=customer_market_data, aes(x=Education)) + 
  geom_bar(fill = 'green')+
  ggtitle('Education Levels')
# majority of customers have received a graduation education level

mt<-ggplot(data=customer_market_data, aes(x=Marital_Status)) + 
  geom_bar(fill = 'red')+
  ggtitle('Marital status')
# majority of customers are married.

cor<-ggplot(data=customer_market_data, aes(x=Education, fill=Marital_Status)) + 
  geom_bar(color = 'white')+
  ggtitle('Education with Marital status')
# no significant correlation between education and marital status

# arranging the plots
library(gridExtra)
grid.arrange(ed,mt,cor, ncol=3, nrow =1)

## The Food Data set
ifood_df <- read_excel("C:/Users/MR MIGWI/Desktop/WORK/PORTFOLIO/Diagnostic Analytics/ifood_df.xlsx")
View(ifood_df)
ifood_df <- na.omit(ifood_df)

# summary statistics
summary(ifood_df)

# missing values
sapply(ifood_df, function(x) sum(is.na(x)))

# correlation among food variables
library(corrplot)
library(Hmisc)
ifood.cor = cor(ifood_df)
ifood.rcor = rcorr(as.matrix(ifood_df))
ifood.rcor
par(mfrow = c(1,1))
corrplot(ifood.cor, method = 'circle')

## Classification Models for prediction
library(caTools)

# Data Partitioning
set.seed(100)  # for reproducibility in results
spl = sample.split(ifood_df$Response, SplitRatio = 0.7)
train = subset(ifood_df, spl==TRUE)
test = subset(ifood_df, spl==FALSE)

print(dim(train)); print(dim(test))

# Build, Predict and Evaluate the Model
model_glm = glm(Response ~ ., family="binomial", data = train)
summary(model_glm)

#Baseline Accuracy
prop.table(table(train$Response))  #85%

# Predictions on the training set
predictTrain = predict(model_glm, data = train, type = "response")

# Confusion matrix on training data
tr<-table(train$Response, predictTrain >= 0.5)
(1305+20)/nrow(train) #Accuracy - 86%
# the algorithm will predict the Yes(1) response for the Response variable.
# the yes response that will be true are 20 while the no responses will be 14.
#Predictions on the test set

predictTest = predict(model_glm, newdata = test, type = "response")

# Confusion matrix on test set
table(test$Response, predictTest >= 0.5)
560/nrow(test) #Accuracy - 85%
# the yes response that will be true are 8 while the no responses will be 1.
# 