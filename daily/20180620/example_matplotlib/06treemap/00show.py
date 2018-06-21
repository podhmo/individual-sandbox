import matplotlib as mlp
import matplotlib.pyplot as plt
import squarify

values = sorted([i ** 3 for i in range(1, 50)], reverse=True)

cmap = mlp.cm.Spectral
norm = mlp.colors.Normalize(vmin=min(values), vmax=max(values))
colors = [cmap(norm(value)) for value in values]

squarify.plot(sizes=values, alpha=.8, color=colors)
plt.axis('off')
plt.show()
