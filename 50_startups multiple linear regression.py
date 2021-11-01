# -*- coding: utf-8 -*-
"""
Created on Mon Nov  1 08:02:25 2021

@author: SAIRAM YERRAMSETTI
"""

# Multiple Linear Regression

#Importing the libraries

import pandas as pd
import numpy as np
import matplotlib as plt


#importing the dataset
import os
os.chdir("C://Users//SAIRAM YERRAMSETTI//Documents//data sets")

dataset = pd.read_csv('50_Startups.csv')

X = dataset.iloc[:,:-1]
y = dataset.iloc[:, 4]


#converting the categorical column

states = pd.get_dummies(X['State'],drop_first=True)

#drop the state column

X= X.drop('State',axis=1)

# concat the dummy variables

X = pd.concat([X,states],axis = 1)



#splitting the dataset into the training set and test set

from sklearn.model_selection import train_test_split
X_train,X_test, y_train, y_test = train_test_split(X, y, test_size =0.2,random_state = 0)

#fitting Multiple Linear Regression to the Training set

from sklearn.linear_model import LinearRegression
regressor = LinearRegression()
regressor.fit(X_train, y_train)

# Predicting the Test set results

y_pred = regressor.predict(X_test)

from sklearn.metrics import r2_score

score = r2_score(y_test,y_pred)
















