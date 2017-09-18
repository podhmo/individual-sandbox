``` python
import os.path
import pandas as pd


anime_csv = os.path.join("sample-data/anime", "anime.csv")
df = pd.read_csv(anime_csv)
```

extract via boolean

``` python
r = df.loc[df['episodes'] == 'Unknown'].head()
print(r)

# anime_id                                    name  \
# 73          21                               One Piece   
# 248        235                         Detective Conan   
# 607       1735                      Naruto: Shippuuden   
# 993      33157  Tanaka-kun wa Itsumo Kedaruge Specials   
# 1226     21639                         Yu☆Gi☆Oh! Arc-V   
# 
#                                                   genre     type episodes  \
# 73    Action, Adventure, Comedy, Drama, Fantasy, Sho...       TV  Unknown   
# 248         Adventure, Comedy, Mystery, Police, Shounen       TV  Unknown   
# 607   Action, Comedy, Martial Arts, Shounen, Super P...       TV  Unknown   
# 993                       Comedy, School, Slice of Life  Special  Unknown   
# 1226                     Action, Fantasy, Game, Shounen       TV  Unknown   
# 
#       rating  members  
# 73      8.58   504862  
# 248     8.25   114702  
# 607     7.94   533578  
# 993     7.72     5400  
# 1226    7.61    17571
```

where()

``` python
r = df.where(df['rating'] < 9.2).head()
print(r)

# anime_id           name                                              genre  \
# 0       NaN            NaN                                                NaN   
# 1       NaN            NaN                                                NaN   
# 2       NaN            NaN                                                NaN   
# 3    9253.0    Steins;Gate                                   Sci-Fi, Thriller   
# 4    9969.0  Gintama&#039;  Action, Comedy, Historical, Parody, Samurai, S...   
# 
#   type episodes  rating   members  
# 0  NaN      NaN     NaN       NaN  
# 1  NaN      NaN     NaN       NaN  
# 2  NaN      NaN     NaN       NaN  
# 3   TV       24    9.17  673572.0  
# 4   TV       51    9.16  151266.0
```

where() with drop NA

``` python
r = df.where(df['rating'] < 9.2).dropna().head()
# no side effect, if you want to mutable effect, use dropna(inplace=True)
print(r)

# anime_id                                               name  \
# 3    9253.0                                        Steins;Gate   
# 4    9969.0                                      Gintama&#039;   
# 5   32935.0  Haikyuu!!: Karasuno Koukou VS Shiratorizawa Ga...   
# 6   11061.0                             Hunter x Hunter (2011)   
# 7     820.0                               Ginga Eiyuu Densetsu   
# 
#                                                genre type episodes  rating  \
# 3                                   Sci-Fi, Thriller   TV       24    9.17   
# 4  Action, Comedy, Historical, Parody, Samurai, S...   TV       51    9.16   
# 5             Comedy, Drama, School, Shounen, Sports   TV       10    9.15   
# 6            Action, Adventure, Shounen, Super Power   TV      148    9.13   
# 7                     Drama, Military, Sci-Fi, Space  OVA      110    9.11   
# 
#     members  
# 3  673572.0  
# 4  151266.0  
# 5   93351.0  
# 6  425855.0  
# 7   80679.0
```

modify values

``` python
import numpy as np  # noqa
df2 = df.copy()
print(df2.loc[74], 'episodes')
df2.loc[74, 'episodes'] = np.nan
print("----------------------------------------")
print(df2.loc[74], 'episodes')

# anime_id                                                  801
# name          Ghost in the Shell: Stand Alone Complex 2nd GIG
# genre       Action, Mecha, Military, Mystery, Police, Sci-...
# type                                                       TV
# episodes                                                   26
# rating                                                   8.57
# members                                                113993
# Name: 74, dtype: object episodes
# ----------------------------------------
# anime_id                                                  801
# name          Ghost in the Shell: Stand Alone Complex 2nd GIG
# genre       Action, Mecha, Military, Mystery, Police, Sci-...
# type                                                       TV
# episodes                                                  NaN
# rating                                                   8.57
# members                                                113993
# Name: 74, dtype: object episodes
```

modify values (multiple)

``` python
print(df2.loc[df['episodes'] == 'Unknown'].head())
df2.loc[df['episodes'] == 'Unknown', 'episodes'] = np.nan
print("----------------------------------------")
print(df2.loc[df['episodes'] == 'Unknown'].head())
print("- original -----------------------------")
print(df.loc[df['episodes'] == 'Unknown'].head())

# anime_id                                    name  \
# 73          21                               One Piece   
# 248        235                         Detective Conan   
# 607       1735                      Naruto: Shippuuden   
# 993      33157  Tanaka-kun wa Itsumo Kedaruge Specials   
# 1226     21639                         Yu☆Gi☆Oh! Arc-V   
# 
#                                                   genre     type episodes  \
# 73    Action, Adventure, Comedy, Drama, Fantasy, Sho...       TV  Unknown   
# 248         Adventure, Comedy, Mystery, Police, Shounen       TV  Unknown   
# 607   Action, Comedy, Martial Arts, Shounen, Super P...       TV  Unknown   
# 993                       Comedy, School, Slice of Life  Special  Unknown   
# 1226                     Action, Fantasy, Game, Shounen       TV  Unknown   
# 
#       rating  members  
# 73      8.58   504862  
# 248     8.25   114702  
# 607     7.94   533578  
# 993     7.72     5400  
# 1226    7.61    17571  
# ----------------------------------------
#       anime_id                                    name  \
# 73          21                               One Piece   
# 248        235                         Detective Conan   
# 607       1735                      Naruto: Shippuuden   
# 993      33157  Tanaka-kun wa Itsumo Kedaruge Specials   
# 1226     21639                         Yu☆Gi☆Oh! Arc-V   
# 
#                                                   genre     type episodes  \
# 73    Action, Adventure, Comedy, Drama, Fantasy, Sho...       TV      NaN   
# 248         Adventure, Comedy, Mystery, Police, Shounen       TV      NaN   
# 607   Action, Comedy, Martial Arts, Shounen, Super P...       TV      NaN   
# 993                       Comedy, School, Slice of Life  Special      NaN   
# 1226                     Action, Fantasy, Game, Shounen       TV      NaN   
# 
#       rating  members  
# 73      8.58   504862  
# 248     8.25   114702  
# 607     7.94   533578  
# 993     7.72     5400  
# 1226    7.61    17571  
# - original -----------------------------
#       anime_id                                    name  \
# 73          21                               One Piece   
# 248        235                         Detective Conan   
# 607       1735                      Naruto: Shippuuden   
# 993      33157  Tanaka-kun wa Itsumo Kedaruge Specials   
# 1226     21639                         Yu☆Gi☆Oh! Arc-V   
# 
#                                                   genre     type episodes  \
# 73    Action, Adventure, Comedy, Drama, Fantasy, Sho...       TV  Unknown   
# 248         Adventure, Comedy, Mystery, Police, Shounen       TV  Unknown   
# 607   Action, Comedy, Martial Arts, Shounen, Super P...       TV  Unknown   
# 993                       Comedy, School, Slice of Life  Special  Unknown   
# 1226                     Action, Fantasy, Game, Shounen       TV  Unknown   
# 
#       rating  members  
# 73      8.58   504862  
# 248     8.25   114702  
# 607     7.94   533578  
# 993     7.72     5400  
# 1226    7.61    17571
```
