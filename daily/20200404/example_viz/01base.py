#!/usr/bin/env python
# coding: utf-8

# In[4]:


import numpy as np
import pandas as pd
from sklearn import datasets
get_ipython().run_line_magic('matplotlib', 'inline')


# In[ ]:





# # setup data

# In[3]:


# irisデータセットサンプル
iris = datasets.load_iris()
# pandas
data_pd = pd.DataFrame(data=iris["data"], columns=iris["feature_names"])
# display(data_pd.head())
# 以降ではirisの"petal length (cm)"カラムを利用する
# list
data_list = [x[3] for x in iris["data"]]
# np.array
data_np = np.array(data_list)

# x軸をGridするためのデータも生成
x_grid = np.linspace(0, max(data_list), num=100)
# データを正規化したヒストグラムを表示する用
weights = np.ones_like(data_list)/float(len(data_list))


# ## setup layout

# In[5]:


import matplotlib.pyplot as plt
plt.style.use("ggplot")


# ## pandas

# In[7]:


data_pd["petal length (cm)"].plot(kind="hist", bins=30, alpha=0.5)
data_pd["petal length (cm)"].plot(kind="kde", secondary_y=True)
plt.show()


# ## scipy

# In[8]:


from scipy.stats import gaussian_kde

kde_model = gaussian_kde(data_list)
y = kde_model(x_grid)

plt.plot(x_grid, y)
plt.hist(data_list, alpha=0.3, bins=20, weights=weights)
plt.show()


# ## sklern

# In[9]:


from sklearn.neighbors import KernelDensity

# default
kde_model = KernelDensity(kernel='gaussian').fit(data_np[:, None])
score = kde_model.score_samples(x_grid[:, None])
# bw値を調整
bw = 0.1
kde_mode_bw = KernelDensity(bandwidth=bw, kernel='gaussian').fit(data_np[:, None])
score_bw = kde_mode_bw.score_samples(x_grid[:, None])

# マイナスが出るので指数関数かます
plt.plot(x_grid, np.exp(score), label="default")
plt.plot(x_grid, np.exp(score_bw), label="origin")
plt.hist(data_list, alpha=0.3, bins=20, weights=weights)
plt.legend()
plt.show()


# In[11]:


# Kearnels
plt.grid(color='white', linestyle='-', linewidth=2)
X_src = np.zeros((1, 1))
x_grid = np.linspace(-3, 3, 1000)
for kernel in ['gaussian', 'tophat', 'epanechnikov',
               'exponential', 'linear', 'cosine']:
    log_dens = KernelDensity(kernel=kernel).fit(X_src).score_samples(x_grid[:, None])
    plt.plot(x_grid, np.exp(log_dens), label=kernel)
plt.legend()
plt.show()


# In[12]:


# plot
for kernel in ['gaussian', 'tophat', 'epanechnikov',
               'exponential', 'linear', 'cosine']:
    kde_modes = KernelDensity(bandwidth=bw, kernel=kernel).fit(data_np[:, None])
    scores = kde_modes.score_samples(x_grid[:, None])
    plt.plot(x_grid, np.exp(scores), label=kernel)
plt.hist(data_list, alpha=0.3, bins=20, weights=weights)
plt.legend()
plt.show()


# In[14]:


from sklearn.model_selection import GridSearchCV

grid = GridSearchCV(KernelDensity(),
                    {'bandwidth': np.linspace(0.1, 1.0, 10)},
                    cv=20)
grid.fit(data_np[:, None])
print(grid.best_params_)

kde_model_best = grid.best_estimator_
best_score = kde_model_best.score_samples(x_grid[:, None])

plt.plot(x_grid, np.exp(best_score), label='bw=%.2f' % kde_model_best.bandwidth)
plt.hist(data_list, alpha=0.3, bins=20, weights=weights)
plt.legend()
plt.show()


# ## 
