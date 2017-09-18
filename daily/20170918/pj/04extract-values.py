import os.path
import pandas as pd


anime_csv = os.path.join("sample-data/anime", "anime.csv")
df = pd.read_csv(anime_csv)

"""extract via boolean"""
r = df.loc[df['episodes'] == 'Unknown'].head()
print(r)

"""where()"""
r = df.where(df['rating'] < 9.2).head()
print(r)

"""where() with drop NA"""
r = df.where(df['rating'] < 9.2).dropna().head()
# no side effect, if you want to mutable effect, use dropna(inplace=True)
print(r)

"""modify values"""
import numpy as np  # noqa
df2 = df.copy()
print(df2.loc[74], 'episodes')
df2.loc[74, 'episodes'] = np.nan
print("----------------------------------------")
print(df2.loc[74], 'episodes')

"""modify values (multiple)"""
print(df2.loc[df['episodes'] == 'Unknown'].head())
df2.loc[df['episodes'] == 'Unknown', 'episodes'] = np.nan
print("----------------------------------------")
print(df2.loc[df['episodes'] == 'Unknown'].head())
print("- original -----------------------------")
print(df.loc[df['episodes'] == 'Unknown'].head())
