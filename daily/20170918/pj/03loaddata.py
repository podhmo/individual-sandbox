import os.path
import pandas as pd
"""
## csv
"""

"""head"""
anime_csv = os.path.join("sample-data/anime", "anime.csv")
df = pd.read_csv(anime_csv)
print(df.head())

"""column index"""
df = pd.read_csv(anime_csv, index_col=0)
print(df.head())

"""column index2"""
df = pd.read_csv(anime_csv, index_col='anime_id')
print(df.head())

"""dtype"""
df = pd.read_csv(anime_csv, dtype={"members": float})
print(df.head())


"""with date time"""
anime_stock_price_csv = os.path.join("sample-data/anime", "anime_stock_price.csv")
df = pd.read_csv(anime_stock_price_csv)
print(df.dtypes)

"""with date time with parse_dates"""
anime_stock_price_csv = os.path.join("sample-data/anime", "anime_stock_price.csv")
df = pd.read_csv(anime_stock_price_csv, parse_dates=['Date'])
print(df.dtypes)


"""changing seperator"""
anime_tsv = os.path.join("sample-data/anime", "anime.tsv")
df = pd.read_csv(anime_tsv, sep='\t')
print(df.head())


"""
## excel
"""

anime_xlsx = os.path.join("sample-data/anime", "anime.xlsx")
df = pd.read_excel(anime_xlsx)
print(df.head())


"""selecting sheet"""
df = pd.read_excel(anime_xlsx, sheetname='Movie')
print(df.head())


"""
## SQL
"""
import sqlite3
anime_db = os.path.join("sample-data/anime", "anime.db")
with sqlite3.connect(anime_db) as c:
    df = pd.read_sql('SELECT * from anime', c)
    print(df.head())

"""
## html
"""
url = "https://docs.python.org/3/py-modindex.html"
tables = pd.read_html(url, index_col=1, flavor="html5lib")
print(tables[0].loc[:, 1:].dropna().head(10))
