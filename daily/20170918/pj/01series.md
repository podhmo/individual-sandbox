``` python
import pandas as pd
```

create Series

``` python
ser = pd.Series([1, 2, 3])
print(ser)

ser = pd.Series([1, 2, 3], index=['a', 'b', 'c'])
print(ser)

# 0    1
# 1    2
# 2    3
# dtype: int64
# a    1
# b    2
# c    3
# dtype: int64
```

select by label

``` python
print(ser['b'])

# 2
```

select by range

``` python
print(ser.loc['b':'c'])

# b    2
# c    3
# dtype: int64
```

select multi item

``` python
print(ser.loc[['a', 'c']])

# a    1
# c    3
# dtype: int64
```

select by index

``` python
print(ser.iloc[1])

# 2
```

select by index range

``` python
print(ser.iloc[1:3])

# b    2
# c    3
# dtype: int64
```

select by bool

``` python
print(ser.loc[[True, False, True]])

# a    1
# c    3
# dtype: int64
```

select by bool (vectorize)

``` python
print(ser.loc[ser != 2])

# a    1
# c    3
# dtype: int64
```
