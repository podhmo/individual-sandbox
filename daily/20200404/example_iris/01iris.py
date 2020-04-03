from sklearn import datasets
import pandas as pd

iris = datasets.load_iris()
df = pd.DataFrame(data=iris["data"], columns=iris["feature_names"])
print(df.describe())
