# Use python environment variables 
#from dotenv import load_dotenv

#load_dotenv()  # take environment variables from .env.

from PI_class_EnbPI import prediction_interval
import utils_EnbPI as util
from matplotlib.lines import Line2D  # For legend handles
import statsmodels as sm
import matplotlib.pyplot as plt
from sklearn.linear_model import RidgeCV, LassoCV
from sklearn.ensemble import RandomForestRegressor
import time
import pandas as pd
import numpy as np
import os
import sys
#import keras

# Convert this R code to Python

index = [2011, 2012, 2013, 2014, 2015, 2016]
s = pd.Series([105545, 125159,  136824, 149012, 166893, 183703], index= index)

rw_auto_ownership = pd.DataFrame(s, columns = ["year","n_registered"])
# rw_auto_ownership <- 
#   data.frame(year = 2011:2016, 
#              n_registered = c()) 


# Read data and initialize parameters
dataSolar_Atl = util.read_data(rw_auto_ownership)
stride = 1
miss_test_idx = []
alpha = 0.1
tot_trial = 10  # For CP methods that randomizes
np.random.seed(98765)
B = 30  # number of bootstrap samples
# Data_name = ['Solar_Atl']
# response_ls = {'Solar_Atl': 'DHI'}
min_alpha = 0.0001
max_alpha = 10
# ridge_cv = RidgeCV(alphas=np.linspace(min_alpha, max_alpha, 10))
# random_forest = RandomForestRegressor(n_estimators=10, criterion='mse',
#                                       bootstrap=False, max_depth=2, n_jobs=-1)
