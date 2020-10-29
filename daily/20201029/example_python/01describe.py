from __future__ import annotations
import typing as t
from vega_datasets import data

if t.TYPE_CHECKING:
    from pandas.core.frame import DataFrame


df: DataFrame = data.cars()
print(df.head())
print(df.describe())
print(df.columns)
grouped_df = df.groupby(by=["Year"]).agg(
    {"Horsepower": ["max", "min", "mean", "std", "count"]}
)
print(grouped_df)

print("")
print("----------------------------------------")
print("")
df: DataFrame = data.iris()
print(df.head())
print(df.describe())
print(df.columns)
grouped_df = df.groupby(by=["species"]).agg(
    {
        "sepalLength": ["max", "min", "mean", "std", "count"],
        "sepalWidth": ["max", "min", "mean", "std", "count"],
    }
)
print(grouped_df)
