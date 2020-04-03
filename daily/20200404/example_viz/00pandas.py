import matplotlib.pyplot as plt
from data import data_pd

import matplotlib.pyplot as plt
plt.style.use("ggplot")

plt.figure(figsize=(14, 7))
data_pd["petal length (cm)"].plot(kind="hist", bins=30, alpha=0.5)
data_pd["petal length (cm)"].plot(kind="kde", secondary_y=True)
plt.show()
