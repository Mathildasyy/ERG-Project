## import packages
# Essentials
import numpy as np
import pandas as pd 
import datetime
import random

# Plots
import seaborn as sns
import matplotlib.pyplot as plt 

# Models
from sklearn.ensemble import RandomForestRegressor, GradientBoostingClassifier, AdaBoostRegressor, BaggingRegressor
from sklearn.kernel_ridge import KernelRidge
from sklearn.linear_model import Ridge, RidgeCV
from sklearn.linear_model import ElasticNet, ElasticNetCV
from sklearn.svm import SVR
from mlxtend.regressor import StackingCVRegressor
from scipy.stats import boxcox_normmax

# Misc
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold, cross_val_score
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import LabelEncoder
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import scale
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import RobustScaler
from sklearn.decomposition import PCA

pd.set_option('display.max_columns', None)

# Ignore useless warnings
import warnings
warnings.filterwarnings(action = 'ignore')
pd.options.display.max_seq_items = 8000
pd.options.display.max_rows = 8000

#import os
#print(os.listdir('/Users/mathilda/Documents/GitHub/ERGProject'))

# Read in the dataset as a dataframe
train =  pd.read_csv('/Users/mathilda/Documents/GitHub/ERGProject/train.csv')
test = pd.read_csv('/Users/mathilda/Documents/GitHub/ERGProject/test.csv')
print(train.shape, test.shape)
d d
train.head()