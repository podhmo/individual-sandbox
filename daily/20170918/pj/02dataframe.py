import pandas as pd

df = pd.DataFrame(
    [[1, 10, 100], [2, 20, 200], [3, 30, 300]],
    index=['r1', 'r2', 'r3'],
    columns=['c1', 'c2', 'c3']
)
print(df)


"""select by label"""
print(df.loc['r2', 'c2'])

"""all columns"""
print(df.loc['r2', :])

"""all rows"""
print(df.loc[:, "c2"])

"""slices"""
print(df.loc[['r1', 'r2'], 'c2':'c3'])

"""iloc"""
print(df.iloc[1:3, [0, 2]])

"""select by rowname"""
print(df["c2"])

"""select by bool (vectorized)"""
print(df.loc[df["c2"] > 10])

"""more complex boolean operation"""
print(df.loc[(df['c1'] > 1) & (df['c3'] < 300)])
