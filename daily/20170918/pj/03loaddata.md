``` python
import os.path
import pandas as pd
```


## csv



head

``` python
anime_csv = os.path.join("sample-data/anime", "anime.csv")
df = pd.read_csv(anime_csv)
print(df.head())

# anime_id                              name  \
# 0     32281                    Kimi no Na wa.   
# 1      5114  Fullmetal Alchemist: Brotherhood   
# 2     28977                          Gintama°   
# 3      9253                       Steins;Gate   
# 4      9969                     Gintama&#039;   
# 
#                                                genre   type episodes  rating  \
# 0               Drama, Romance, School, Supernatural  Movie        1    9.37   
# 1  Action, Adventure, Drama, Fantasy, Magic, Mili...     TV       64    9.26   
# 2  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.25   
# 3                                   Sci-Fi, Thriller     TV       24    9.17   
# 4  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.16   
# 
#    members  
# 0   200630  
# 1   793665  
# 2   114262  
# 3   673572  
# 4   151266
```

column index

``` python
df = pd.read_csv(anime_csv, index_col=0)
print(df.head())

# name  \
# anime_id                                     
# 32281                       Kimi no Na wa.   
# 5114      Fullmetal Alchemist: Brotherhood   
# 28977                             Gintama°   
# 9253                           Steins;Gate   
# 9969                         Gintama&#039;   
# 
#                                                       genre   type episodes  \
# anime_id                                                                      
# 32281                  Drama, Romance, School, Supernatural  Movie        1   
# 5114      Action, Adventure, Drama, Fantasy, Magic, Mili...     TV       64   
# 28977     Action, Comedy, Historical, Parody, Samurai, S...     TV       51   
# 9253                                       Sci-Fi, Thriller     TV       24   
# 9969      Action, Comedy, Historical, Parody, Samurai, S...     TV       51   
# 
#           rating  members  
# anime_id                   
# 32281       9.37   200630  
# 5114        9.26   793665  
# 28977       9.25   114262  
# 9253        9.17   673572  
# 9969        9.16   151266
```

column index2

``` python
df = pd.read_csv(anime_csv, index_col='anime_id')
print(df.head())

# name  \
# anime_id                                     
# 32281                       Kimi no Na wa.   
# 5114      Fullmetal Alchemist: Brotherhood   
# 28977                             Gintama°   
# 9253                           Steins;Gate   
# 9969                         Gintama&#039;   
# 
#                                                       genre   type episodes  \
# anime_id                                                                      
# 32281                  Drama, Romance, School, Supernatural  Movie        1   
# 5114      Action, Adventure, Drama, Fantasy, Magic, Mili...     TV       64   
# 28977     Action, Comedy, Historical, Parody, Samurai, S...     TV       51   
# 9253                                       Sci-Fi, Thriller     TV       24   
# 9969      Action, Comedy, Historical, Parody, Samurai, S...     TV       51   
# 
#           rating  members  
# anime_id                   
# 32281       9.37   200630  
# 5114        9.26   793665  
# 28977       9.25   114262  
# 9253        9.17   673572  
# 9969        9.16   151266
```

dtype

``` python
df = pd.read_csv(anime_csv, dtype={"members": float})
print(df.head())

# anime_id                              name  \
# 0     32281                    Kimi no Na wa.   
# 1      5114  Fullmetal Alchemist: Brotherhood   
# 2     28977                          Gintama°   
# 3      9253                       Steins;Gate   
# 4      9969                     Gintama&#039;   
# 
#                                                genre   type episodes  rating  \
# 0               Drama, Romance, School, Supernatural  Movie        1    9.37   
# 1  Action, Adventure, Drama, Fantasy, Magic, Mili...     TV       64    9.26   
# 2  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.25   
# 3                                   Sci-Fi, Thriller     TV       24    9.17   
# 4  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.16   
# 
#     members  
# 0  200630.0  
# 1  793665.0  
# 2  114262.0  
# 3  673572.0  
# 4  151266.0
```

with date time

``` python
anime_stock_price_csv = os.path.join("sample-data/anime", "anime_stock_price.csv")
df = pd.read_csv(anime_stock_price_csv)
print(df.dtypes)

# Date               object
# TOEI ANIMATION    float64
# IG Port           float64
# dtype: object
```

with date time with parse_dates

``` python
anime_stock_price_csv = os.path.join("sample-data/anime", "anime_stock_price.csv")
df = pd.read_csv(anime_stock_price_csv, parse_dates=['Date'])
print(df.dtypes)

# Date              datetime64[ns]
# TOEI ANIMATION           float64
# IG Port                  float64
# dtype: object
```

changing seperator

``` python
anime_tsv = os.path.join("sample-data/anime", "anime.tsv")
df = pd.read_csv(anime_tsv, sep='\t')
print(df.head())

# anime_id                              name  \
# 0     32281                    Kimi no Na wa.   
# 1      5114  Fullmetal Alchemist: Brotherhood   
# 2     28977                          Gintama°   
# 3      9253                       Steins;Gate   
# 4      9969                     Gintama&#039;   
# 
#                                                genre   type episodes  rating  \
# 0               Drama, Romance, School, Supernatural  Movie        1    9.37   
# 1  Action, Adventure, Drama, Fantasy, Magic, Mili...     TV       64    9.26   
# 2  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.25   
# 3                                   Sci-Fi, Thriller     TV       24    9.17   
# 4  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.16   
# 
#    members  
# 0   200630  
# 1   793665  
# 2   114262  
# 3   673572  
# 4   151266
```


## excel


``` python
anime_xlsx = os.path.join("sample-data/anime", "anime.xlsx")
df = pd.read_excel(anime_xlsx)
print(df.head())

# anime_id                                               name  \
# 0     32281                                     Kimi no Na wa.   
# 1     15335  Gintama Movie: Kanketsu-hen - Yorozuya yo Eien...   
# 2     28851                                     Koe no Katachi   
# 3       199                      Sen to Chihiro no Kamikakushi   
# 4     12355                       Ookami Kodomo no Ame to Yuki   
# 
#                                                genre   type episodes  rating  \
# 0               Drama, Romance, School, Supernatural  Movie        1    9.37   
# 1  Action, Comedy, Historical, Parody, Samurai, S...  Movie        1    9.10   
# 2                             Drama, School, Shounen  Movie        1    9.05   
# 3                     Adventure, Drama, Supernatural  Movie        1    8.93   
# 4                             Fantasy, Slice of Life  Movie        1    8.84   
# 
#    members  
# 0   200630  
# 1    72534  
# 2   102733  
# 3   466254  
# 4   226193
```

selecting sheet

``` python
df = pd.read_excel(anime_xlsx, sheetname='Movie')
print(df.head())

# anime_id                                               name  \
# 0     32281                                     Kimi no Na wa.   
# 1     15335  Gintama Movie: Kanketsu-hen - Yorozuya yo Eien...   
# 2     28851                                     Koe no Katachi   
# 3       199                      Sen to Chihiro no Kamikakushi   
# 4     12355                       Ookami Kodomo no Ame to Yuki   
# 
#                                                genre   type episodes  rating  \
# 0               Drama, Romance, School, Supernatural  Movie        1    9.37   
# 1  Action, Comedy, Historical, Parody, Samurai, S...  Movie        1    9.10   
# 2                             Drama, School, Shounen  Movie        1    9.05   
# 3                     Adventure, Drama, Supernatural  Movie        1    8.93   
# 4                             Fantasy, Slice of Life  Movie        1    8.84   
# 
#    members  
# 0   200630  
# 1    72534  
# 2   102733  
# 3   466254  
# 4   226193
```


## SQL


``` python
import sqlite3
anime_db = os.path.join("sample-data/anime", "anime.db")
with sqlite3.connect(anime_db) as c:
    df = pd.read_sql('SELECT * from anime', c)
    print(df.head())

# anime_id                              name  \
# 0     32281                    Kimi no Na wa.   
# 1      5114  Fullmetal Alchemist: Brotherhood   
# 2     28977                          Gintama°   
# 3      9253                       Steins;Gate   
# 4      9969                     Gintama&#039;   
# 
#                                                genre   type episodes  rating  \
# 0               Drama, Romance, School, Supernatural  Movie        1    9.37   
# 1  Action, Adventure, Drama, Fantasy, Magic, Mili...     TV       64    9.26   
# 2  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.25   
# 3                                   Sci-Fi, Thriller     TV       24    9.17   
# 4  Action, Comedy, Historical, Parody, Samurai, S...     TV       51    9.16   
# 
#    members  
# 0   200630  
# 1   793665  
# 2   114262  
# 3   673572  
# 4   151266
```


## html


``` python
url = "https://docs.python.org/3/py-modindex.html"
tables = pd.read_html(url, index_col=1, flavor="html5lib")
print(tables[0].loc[:, 1:].dropna().head(10))

# 2
# 1                                                               
# __future__                          Future statement definitions
# __main__       The environment where the top-level script is ...
# _dummy_thread        Drop-in replacement for the _thread module.
# _thread                                 Low-level threading API.
# abc                 Abstract base classes according to PEP 3119.
# aifc           Read and write audio files in AIFF or AIFC for...
# argparse       Command-line option and argument parsing library.
# array          Space efficient arrays of uniformly typed nume...
# ast               Abstract Syntax Tree classes and manipulation.
# asynchat       Support for asynchronous command/response prot...
```
