Walmart_data = read.csv("Walmart_Store_sales.csv",header = TRUE, sep = ",")

#Data Understanding
View(Walmart_data)
dim(Walmart_data)
head(Walmart_data)
tail(Walmart_data)
names(Walmart_data)
str(Walmart_data)
summary(Walmart_data)

#pairs plot 
pairs(Walmart_data[,c(1:8)]
      ,col='blue'
      ,main='PAIR PLOT'
)


Walmart_data_1<-read.csv("Walmart_Store_sales.csv",header = TRUE, sep = ",")

#creating new variable in place of date var as days
Walmart_data_1$Updated_Date <- Walmart_data_1$Date 
Walmart_data_1$Updated_Date<- as.numeric(Walmart_data_1$Updated_Date)
class(Walmart_data_1$Updated_Date)

library(dplyr)

Walmart_data_2<-arrange(Walmart_data_1,Updated_Date)
View(Walmart_data_2)

sum(is.na(Walmart_data_2))
library(caTools)
set.seed(1)

#splitting dataset into training and testing
sample <- sample.split(Walmart_data_2$Weekly_Sales,SplitRatio = 0.7)
sample
train_data = subset(Walmart_data_2,sample == TRUE)
test_data = subset(Walmart_data_2,sample == FALSE)

#building multilinear model
model1 <- lm(Weekly_Sales~ Store+Weekly_Sales+Holiday_Flag+Temperature+ Fuel_Price+CPI+Unemployment+Updated_Date,data = train_data)
summary(model1)

#Rebilding model for improvising
model2 <- lm(Weekly_Sales~ Store+Weekly_Sales+Holiday_Flag+Temperature+CPI+Unemployment ,data = train_data)
summary(model2)

#VIF for every predictor
library(car)
vif(model2)

#RESIDUAL ANALYSIS

#overview of model2
par(mfrow=c(2,2))
plot(model2)

#relation between response Y and the Regression
plot(model2,which = 1)

#errors distribution is NORMAL
plot(model2,which = 2)

#error term has constant variance
plot(model2,which = 3)

#error term has zero mean
mean(resid(model2))

#errors are uncorelated
plot(model2$residuals)

#Outliers and Leverage
plot(model2, which = 5)

#predicting sales
prediction <- predict(model2,test_data)
head(prediction)
prediction_1 <- data.frame(prediction) #storing predicted data as data frame

Final_data = cbind(test_data,prediction_1) #merging predicted data and testing data
View(Final_data)
head(Final_data,10)

library(caret)
R_sq <- R2(prediction,test_data$Weekly_Sales)
RMSE <- RMSE(prediction, test_data$Weekly_Sales)
MAE <- MAE(prediction, test_data$Weekly_Sales)
print(c(R_sq, RMSE, MAE))

pred_error_rate <- RMSE / mean(test_data$Weekly_)
pred_error_rate


write.csv(Final_data,"walmart_Model.csv")