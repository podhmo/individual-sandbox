import numpy as np
import pandas as pd
from sklearn import datasets

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
