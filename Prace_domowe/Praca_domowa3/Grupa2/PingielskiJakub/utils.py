import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from imblearn.pipeline import Pipeline

from sklearn.model_selection import cross_validate, RepeatedStratifiedKFold
from sklearn.preprocessing import PowerTransformer

from skopt import BayesSearchCV
from skopt.space import Real, Categorical, Integer

from xgboost import XGBClassifier
from catboost import CatBoostClassifier
from lightgbm import LGBMClassifier

from sklearn.experimental import enable_hist_gradient_boosting  
from sklearn.ensemble import HistGradientBoostingClassifier

from sklearn.ensemble import (RandomForestClassifier, 
                              AdaBoostClassifier, 
                              ExtraTreesClassifier)

from sklearn.linear_model import LogisticRegression
from sklearn.neural_network import MLPClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC

from IPython.display import Audio, display

class InvisibleAudio(Audio):
    def _repr_html_(self):
        audio = super()._repr_html_()
        audio = audio.replace('<audio', f'<audio onended="this.parentNode.removeChild(this)"')
        return f'<div style="display:none">{audio}</div>'


def done(): display(InvisibleAudio(filename='./panie_model_zrobiony.mp3', autoplay=True))


def train_validate(model,
                   preprocess,
                   param_grid,
                   X_train,
                   y_train,
                   metric='roc_auc',
                   n_iter=20):


    
    final_model = Pipeline([
      ('upsampling', preprocess['upsampling']),
      ('transform', preprocess['transform']), 
      ('classifier', model)
    ])

    model_search = BayesSearchCV(estimator=final_model,
                                 search_spaces=param_grid,
                                 scoring=metric,
                                 n_iter=n_iter,
                                 n_jobs=-1).fit(X_train, y_train)

    print("Parameters search completed!")

    best_model = model_search.best_estimator_

    best_model_scores = cross_validate(best_model,
                                       X_train,
                                       y_train,
                                       cv=RepeatedStratifiedKFold(n_repeats=5),
                                       scoring=metric,
                                       n_jobs=-1)

    print("Cross validation on best model completed!")

    best_model_scores = best_model_scores["test_score"]

    mean_valid_score = np.round(np.mean(best_model_scores), 4)

    print("Mean validation score: ", mean_valid_score)
    
    done() 

    return best_model, best_model_scores, mean_valid_score


def plot_results(MODELS_SUMMARY):
    
    RESULTS = pd.DataFrame(MODELS_SUMMARY.model_scores.apply(pd.Series).T)    
    
    RESULTS.columns = MODELS_SUMMARY.model_name.values
    
    sorted_index = RESULTS.median().sort_values().index
    
    RESULTS = RESULTS[sorted_index]
    
    plt.figure(figsize=(15, 6))
    sns.set_style("whitegrid")

    sns.boxplot(data=RESULTS, color='steelblue')
    sns.swarmplot(data=RESULTS ,color=".25").set_title('Comparision of CV AUC distributions')


def plot_coefficients(model):
    
    coefficients = pd.DataFrame({
    'feature' : X_train.columns,
    'coefficient' : best_model[2].coef_.ravel().tolist()
    })
    
    coefficients.sort_values("coefficient", inplace=True)
    
    plt.figure(figsize=(15, 6))
    sns.barplot(y='feature',
            x='coefficient', 
            data=coefficients,
            color="steelblue").set_title('Coefficients in logistic regression')
    

def create_params():

  param_grid_xgb = {
      'classifier__learning_rate'     :  Real(0.1, 1.0, prior='log-uniform'),
      'classifier__max_depth'         :  Integer(1, 20),
      'classifier__gamma'             :  Real(0.01, 1.0, prior='log-uniform'),
      'classifier__subsample'         :  Real(0.1, 1.0, prior='uniform'),
  }
 
  param_grid_lgbm = {
      'classifier__num_leaves'        : Integer(10, 150),
      'classifier__learning_rate'     : Real(0.01, 1),
      'classifier__max_depth'         : Integer(1, 30),
      'classifier__feature_fraction'  : Real(0.1, 1),
      'classifier__subsample'         : Real(0.1, 1)
  }

  param_grid_hgb = {
      'classifier__learning_rate'     : Real(0.001, 0.1, prior='log-uniform'),
      'classifier__max_leaf_nodes'    : Integer(2, 60),
      'classifier__max_depth'         : Integer(2, 50),
      'classifier__min_samples_leaf'  : Integer(2, 20),
      'classifier__l2_regularization' : Real(1, 100)
  }

  param_grid_extra = {
      'classifier__n_estimators'      : Integer(100, 500),
      'classifier__min_samples_leaf'  : Integer(1, 10),
      'classifier__max_depth'         : Integer(1, 30),
      'classifier__max_features'      : Integer(2, 10),
      'classifier__class_weight'      : ['balanced'],
      'classifier__criterion'         : ['gini', 'entropy']
  }

  param_grid_log = {
      'classifier__penalty'           : ['l1', 'l2'],
      'classifier__C'                 : Real(0.01, 100),
      'classifier__class_weight'      : ['balanced'],
      'classifier__solver'            : ['saga']
  }

  param_grid_rf = {
      'classifier__n_estimators'      : Integer(100, 500),
      'classifier__min_samples_leaf'  : Integer(1, 10),
      'classifier__max_depth'         : Integer(1, 30),
      'classifier__max_features'      : Integer(2, 10),
      'classifier__class_weight'      : ['balanced'],
      'classifier__criterion'         : ['gini', 'entropy']
  }

  param_grid_svm = {
      'classifier__C'                 : Real(1000, 100000),
      'classifier__gamma'             : Real(0.000000001, 0.0001, prior='log-uniform'),
      'classifier__class_weight'      : ['balanced']
  }

  param_grid_nn = {
      'classifier__activation'        : ['tanh', 'relu'],
      'classifier__solver'            : ['sgd', 'adam'],
      'classifier__alpha'             : Real(0.01, 10),
      'classifier__learning_rate'     : ['constant','adaptive'],
  }

  param_grid_knn = {
      'classifier__n_neighbors'       : Integer(1, 150)
  }

  param_grid_tree = {
      'classifier__min_samples_split' : Integer(2, 20),
      'classifier__max_depth'         : Integer(1, 20),
      'classifier__class_weight'      : ['balanced'],
  }

  param_grid_ada = {
      'classifier__n_estimators'      : Integer(20, 200),
      'classifier__learning_rate'     : Real(0.001, 0.1, prior='log-uniform')
  }

  param_grid_cat = {
      'classifier__learning_rate'     : Real(0.001, 0.1, prior='log-uniform'),
      'classifier__depth'             : Integer(1, 6),
      'classifier__l2_leaf_reg'       : Real(1, 100),
      'classifier__silent'            : [True]
  }

  PARAMS = {
    'XGBOOST' : param_grid_xgb,
    'LGBM'    : param_grid_lgbm, 
    'HGB'     : param_grid_hgb,
    'EXTRA'   : param_grid_extra,
    'LOG'     : param_grid_log,
    'RF'      : param_grid_rf,
    'SVM'     : param_grid_svm,
    'NN'      : param_grid_nn,
    'KNN'     : param_grid_knn,
    'TREE'    : param_grid_tree,
    'ADA'     : param_grid_ada,
    'CAT'     : param_grid_cat
  }

  return PARAMS


def create_models():

    MODELS = {
        'LOG'     : LogisticRegression(),
        'LGBM'    : LGBMClassifier(),
        'ADA'     : AdaBoostClassifier(),
        'NN'      : MLPClassifier(),
        'XGBOOST' : XGBClassifier(),
        'HGB'     : HistGradientBoostingClassifier(),
        'RF'      : RandomForestClassifier(),
        'SVM'     : SVC(),
        'EXTRA'   : ExtraTreesClassifier(),
        'KNN'     : KNeighborsClassifier()     
    }
    
    return MODELS


def create_preprocess():
    
    PREPROCESS = {
        'upsampling'      : None,
        'transform'       : PowerTransformer(),
        'pca'             : None
    }
    
    return PREPROCESS


def create_models_summary(MODELS, PARAMS, PREPROCESS, X_train, y_train):
    
    MODELS_SUMMARY = pd.DataFrame(columns=['model_name', 'best_model', 'model_scores', 'model_mean_score'])
        
    for model_name, model in MODELS.items():
    
        print("creating_model: ", model_name)

        best_model, model_scores, model_mean_score = train_validate(
            model=model,
            preprocess=PREPROCESS,
            param_grid=PARAMS[model_name],
            X_train=X_train,
            y_train=y_train)

        ROW = pd.DataFrame([{"model_name"       : model_name, 
                             "best_model"       : best_model,
                             "model_scores"     : model_scores,
                             "model_mean_score" : model_mean_score
                           }])

        MODELS_SUMMARY = MODELS_SUMMARY.append(ROW, ignore_index=True)
        
    return MODELS_SUMMARY