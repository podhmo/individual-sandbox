``` python
import os.path
import pandas as pd

anime_csv = os.path.join("sample-data/anime", "anime.csv")
r = pd.read_csv(anime_csv).head()
print(r)

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
