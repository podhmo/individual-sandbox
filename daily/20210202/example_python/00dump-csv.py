import logging
import vega_datasets

df = vega_datasets.data.iris()
df.to_csv("iris.csv")
