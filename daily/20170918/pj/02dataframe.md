``` python
import pandas as pd

df = pd.DataFrame(
    [[1, 10, 100], [2, 20, 200], [3, 30, 300]],
    index=['r1', 'r2', 'r3'],
    columns=['c1', 'c2', 'c3']
)
print(df)

# c1  c2   c3
# r1   1  10  100
# r2   2  20  200
# r3   3  30  300
```

select by label

``` python
print(df.loc['r2', 'c2'])

# 20
```

all columns

``` python
print(df.loc['r2', :])

# c1      2
# c2     20
# c3    200
# Name: r2, dtype: int64
```

all rows

``` python
print(df.loc[:, "c2"])

# r1    10
# r2    20
# r3    30
# Name: c2, dtype: int64
```

slices

``` python
print(df.loc[['r1', 'r2'], 'c2':'c3'])

# c2   c3
# r1  10  100
# r2  20  200
```

iloc

``` python
print(df.iloc[1:3, [0, 2]])

# c1   c3
# r2   2  200
# r3   3  300
```

select by rowname

``` python
print(df["c2"])

# r1    10
# r2    20
# r3    30
# Name: c2, dtype: int64
```

select by bool (vectorized)

``` python
print(df.loc[df["c2"] > 10])

# c1  c2   c3
# r2   2  20  200
# r3   3  30  300
```

more complex boolean operation

``` python
print(df.loc[(df['c1'] > 1) & (df['c3'] < 300)])

# c1  c2   c3
# r2   2  20  200
```
