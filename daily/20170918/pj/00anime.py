import os.path
import pandas as pd

anime_csv = os.path.join("sample-data/anime", "anime.csv")
r = pd.read_csv(anime_csv).head()
print(r)
