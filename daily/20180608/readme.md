## python csv merge

mergeするときにはpandas使ったほうが良いか


```python
import pandas

df0 = pandas.read_csv("data/members.csv")
df1 = pandas.read_csv("data/groups.csv")

df0.merge(df1, how="left", on="groupId")
```

一部だけマージしたいときはどうだろう。

```python
def extract_main_metrics(df, endtoken):
    ks = []
    for k in df.columns:
        ks.append(k)
        if k == endtoken:
            break
    return df[ks]


df0 = pandas.read_csv("data/members.csv")
mdf0 = extract_main_metrics(df0, "name")
df1 = pandas.read_csv("data/groups.csv")
mdf1 = extract_main_metrics(df1, "name")

mdf0.merge(mdf1, how="left", on="groupId")
```

- http://pandas.pydata.org/pandas-docs/stable/generated/pandas.read_csv.html
- https://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.merge.html

## python csv

csvに出力する時に欠損値の表記は `restval` の値で変えられそう

```python
import csv
import sys

w = csv.DictWriter(sys.stdout, ["x", "y"], restval="-")

w.writeheader()
w.writerows(rows)
```

https://docs.python.org/ja/3/library/csv.html#csv.DictReader
