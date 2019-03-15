####We are now importing the data into "R-Studio"

trainDataSet <- read.csv("D:\\Data Science\\End to end\\Train_UWu5bXk.csv",header = T, sep = ',')
testDataSet <- read.csv("D:\\Data Science\\End to end\\Test_u94Q5KV.csv",header = T, sep = ',')


####to get the summary of data


##summary actually gives you a big picture of what the table contains.
##for eg. it will take a column and describe about it on high level.
summary(trainDataSet)
str(trainDataSet)
##to check the skweness , we need to install the package named "psych"

install.packages('psych')
library('psych')

describe(trainDataSet) ##describe will give us the skweness

#*****************************************#str(trainDataSet)

##to check the extreme values , install a package named " hmisc"


install.packages('hmisc')
library(Hmisc)

describe(trainDataSet) ## this describe in hmisc will give us the extreme values


boxplot(trainDataSet$Item_Weight)
boxplot(trainDataSet$Item_Visibility)


#### to check the missung values

sum(is.na(trainDataSet$Item_Weight))
which(is.na(trainDataSet$Item_Weight))

####to replace the missing values

trainDataSet$Item_Weight <- ifelse(is.na(trainDataSet$Item_Weight),median(trainDataSet$Item_Weight,na.rm = T),trainDataSet$Item_Weight)
sum(is.na(trainDataSet$Item_Weight))


sum(is.na(trainDataSet$Outlet_Size))


####is.na should be applied only for numbers. If we are going to replace for STRINGs or characters,
####we have to give just a "" or " "

trainDataSet$Outlet_Size[trainDataSet$Outlet_Size==''] <- "Medium"

table(trainDataSet$Outlet_Size)


table(trainDataSet$Item_Fat_Content)


trainDataSet$Item_Fat_Content[trainDataSet$Item_Fat_Content=='LF'] <- "Low Fat"
trainDataSet$Item_Fat_Content[trainDataSet$Item_Fat_Content=='low fat'] <- "Low Fat"
trainDataSet$Item_Fat_Content[trainDataSet$Item_Fat_Content=='reg'] <- "Regular"

table(trainDataSet$Item_Fat_Content)


####To create a new variable for Establishment 

trainDataSet$yearsOfEstablishment <- 2018 - trainDataSet$Outlet_Establishment_Year

summary(trainDataSet$yearsOfEstablishment)
str(trainDataSet$yearsOfEstablishment)

####to find the co relation

cor(trainDataSet$Item_Weight,trainDataSet$Item_Visibility)

#### to know the columns that are there in the data
names(trainDataSet)

#### to know the linear model, we will need to use lm fn. we need to give only continous variables

str(trainDataSet)

linearModel <- lm(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_MRP+yearsOfEstablishment, data = trainDataSet)
summary(linearModel)


#### to verify or get the predicted value, we need to pass the predict()fn

trainDataSet$predictedValueFromLinearModel <- predict(linearModel,trainDataSet)

trainDataSet$residualResults <- linearModel$residuals #Actual - Predicted is the residal formula


names(trainDataSet)

model2 <- lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+yearsOfEstablishment, data = trainDataSet)
summary(model2)

trainDataSet$predictedValueFromLinearMode2WithFullVariables <- predict(model2,trainDataSet)


rmse <- sqrt(mean((trainDataSet$Item_Outlet_Sales - trainDataSet$predictedValueFromLinearMode2WithFullVariables)^2))
print(rmse)

####performing transformation to reduce the RMSE value
trainDataSet$log_weight <- log(trainDataSet$Item_Weight)
trainDataSet$log_MRP <- log(trainDataSet$Item_MRP)
trainDataSet$log_yearsOfPublish <- log(trainDataSet$yearsOfEstablishment)


model3 <- lm(Item_Outlet_Sales~log_weight+Item_Fat_Content+Item_Visibility+Item_Type+log_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+log_yearsOfPublish, data = trainDataSet)
summary(model3)

trainDataSet$predictedValueFromLinearMode3WithFullVariablesWithTrasformation <- predict(model3,trainDataSet) 




rmse_PredictWithTranform <- sqrt(mean((trainDataSet$Item_Outlet_Sales - trainDataSet$predictedValueFromLinearMode3WithFullVariablesWithTrasformation)^2))
print(rmse_PredictWithTranform)



write.csv(trainDataSet,'manipulated_Train.csv')
getwd()
