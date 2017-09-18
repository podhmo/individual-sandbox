import pandas as pd

"""create Series"""
ser = pd.Series([1, 2, 3])
print(ser)

ser = pd.Series([1, 2, 3], index=['a', 'b', 'c'])
print(ser)

"""select by label"""
print(ser['b'])

"""select by range"""
print(ser.loc['b':'c'])

"""select multi item"""
print(ser.loc[['a', 'c']])

"""select by index"""
print(ser.iloc[1])

"""select by index range"""
print(ser.iloc[1:3])

"""select by bool"""
print(ser.loc[[True, False, True]])

"""select by bool (vectorize)"""
print(ser.loc[ser != 2])
