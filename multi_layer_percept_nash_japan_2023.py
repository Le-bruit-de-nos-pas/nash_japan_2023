import shap
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPRegressor
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import train_test_split



df = pd.read_csv("NASH_trial_class.txt", sep=",")

# df = df.groupby('Dx').apply(lambda x: x.sample(1000)).reset_index(drop=True)

df['Dx'].value_counts()

df

y = df['Dx']

X =  df.loc[:, df.columns != 'Dx'].values

features = df.columns[df.columns != 'Dx']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33, random_state=42)


model = make_pipeline(
    StandardScaler(),
    MLPRegressor(hidden_layer_sizes=(20,10,5),
                 activation='logistic',
                 max_iter=10000,
                 learning_rate='invscaling',
                 random_state=0)
)

model.fit(X_train,y_train)

pd.concat(
    [
    pd.DataFrame(y_test).reset_index(drop=True), 
    pd.DataFrame(model.predict(X_test))
    ], axis=1
    ).groupby(['Dx']).mean()

X_test

pd.DataFrame(model.predict(X_test))

pd.DataFrame(model.predict(X_test)).value_counts()

sns.distplot(pd.DataFrame(model.predict(X_test)), hist=True, kde=True, 
             bins=int(3300/100), color = 'darkblue', 
             hist_kws={'edgecolor':'black'},
             kde_kws={'linewidth': 4})


explainer = shap.KernelExplainer(model.predict,X_train)

shap_values = explainer.shap_values(X_test, nsamples=100)


shap.summary_plot(shap_values,X_test,feature_names=features)


shap.initjs()

shap.force_plot(explainer.expected_value,
                shap_values[0 ,:],
                X_test[0,:],
                feature_names=features)



shap.initjs()

shap.force_plot(explainer.expected_value,
                shap_values[41 ,:],
                X_test[41,:],
                feature_names=features)



shap.initjs()

shap.force_plot(explainer.expected_value,
                shap_values[39 ,:],
                X_test[39,:],
                feature_names=features)



shap.initjs()

shap.force_plot(explainer.expected_value,
                shap_values[51 ,:],
                X_test[51,:],
                feature_names=features)


matplotlib.pyplot.plot(model.loss_curve_)