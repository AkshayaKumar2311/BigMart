# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import pandas as pd
import numpy as np
import os

#Set Working directory
os.chdir("D:\\Data Science\\Python\\Big Mart")

#import Train and test data
trainData = pd.read_csv("Train_Data.csv")
testData = pd.read_csv("Test_Data.csv")


#To decribe the data
summary = trainData.describe()
trainData.info()

#To know the column name :

trainData.columns.values
# =============================================================================
# Out put of the above command is 
# array(['Item_Identifier', 'Item_Weight', 'Item_Fat_Content',
#        'Item_Visibility', 'Item_Type', 'Item_MRP', 'Outlet_Identifier',
#        'Outlet_Establishment_Year', 'Outlet_Size', 'Outlet_Location_Type',
#        'Outlet_Type', 'Item_Outlet_Sales'], dtype=object)
# =============================================================================

#To know for categorical Variables
trainData['Outlet_Size'].value_counts()

#Checking for the missing values

(trainData.isnull().sum())

#To impute values 

ItemWeight = np.where(trainData['Item_Weight'].isnull(),12.60,trainData["Item_Weight"])

trainData["Item_Weight"] = ItemWeight

OutletSize = np.where(trainData['Outlet_Size'].isnull(),'Medium',trainData['Outlet_Size'])

trainData['Outlet_Size'] = OutletSize


#Importing to encode the categorical values

from sklearn.preprocessing import LabelEncoder

le = LabelEncoder()
trainData['Item_Type'] = le.fit_transform(trainData['Item_Type'])

#Inconsistent Value
trainData['Item_Type'].value_counts()

trainData['Item_Fat_Content'].value_counts()

trainData['Item_Fat_Content'].replace('LF','Low Fat',inplace=True)

trainData['Item_Fat_Content'].replace('low fat','Low Fat',inplace=True)

trainData['Item_Fat_Content'].replace('reg','Regular',inplace=True)

trainData['Item_Fat_Content'] = le.fit_transform(trainData['Item_Fat_Content'])

trainData['Outlet_Size'].value_counts()

trainData['Outlet_Location_Type'].value_counts()

trainData['Outlet_Type'].value_counts()

trainData['Outlet_Size'] = le.fit_transform(trainData['Outlet_Size'])

trainData['Outlet_Location_Type'] = le.fit_transform(trainData['Outlet_Location_Type'])

trainData['Outlet_Type'] = le.fit_transform(trainData['Outlet_Type'])

CurrentYear = 2018

yearOfEstablishment = 2018 - trainData['Outlet_Establishment_Year']

trainData['Outlet_Establishment_Year'] = yearOfEstablishment

#divide the data into dependent and independent variables

Y = trainData['Item_Outlet_Sales']

X = trainData[['Item_Weight','Item_Fat_Content','Item_Visibility','Item_Type','Item_MRP',
              'Outlet_Establishment_Year','Outlet_Size','Outlet_Location_Type','Outlet_Type']]

import statsmodels.api as sm

Linearmodel = sm.OLS(Y,X).fit()

Linearmodel.summary()



#Linear Regression

from sklearn import linear_model
lm= linear_model.LinearRegression()
LinearModelUsingSKlearn = lm.fit(X,Y)
preds_LR = LinearModelUsingSKlearn.predict(X)

from sklearn.metrics import mean_squared_error
rmse_LR = np.sqrt(mean_squared_error(Y,preds_LR))

print(rmse_LR)




#########################Random Forest#######################


from sklearn.ensemble import RandomForestRegressor
rf= RandomForestRegressor(n_estimators=500)
randomForest_Model = rf.fit(X,Y)
predictModel_rf = randomForest_Model.predict(X)
rmse_rfModel = np.sqrt(mean_squared_error(Y,predictModel_rf))
print(rmse_rfModel)


######################Support Vector Machine##############

from sklearn.svm import SVR
svr_rbf = SVR(kernel='rbf')
SupportVectorMachine_Model = svr_rbf.fit(X,Y)
preds_svr = SupportVectorMachine_Model.predict(X)
rmse_svm = np.sqrt(mean_squared_error(Y,preds_svr))
print(rmse_svm)


###############Neural Network ###############################

from sklearn.neural_network import MLPRegressor
MLP = MLPRegressor(activation='relu', hidden_layer_sizes=(10,10,10),max_iter=100)
neuralNetwors_Model = MLP.fit(X,Y)
nnPredict = neuralNetwors_Model.predict(X)
rmse_nn = np.sqrt(mean_squared_error(Y,nnPredict))
print(rmse_nn)


#######To write in csv file
# =============================================================================
# 
# output=pd.DataFrame(outputvariable)
# 
# output.to_csv('output_Submission.csv')
# =============================================================================
