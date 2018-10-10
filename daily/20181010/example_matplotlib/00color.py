import matplotlib.colors as colors
import matplotlib.cm as cm

print(colors.to_hex("r"))
print(colors.to_rgba_array("r"))
print(colors.to_rgba("r"))
print(colors.to_rgba([1, 1, 1]))


print(cm.get_cmap("Blues").N)
print(cm.get_cmap("Blues")(0))
print(cm.get_cmap("Blues")(10))
# print(dir(cm.get_cmap("Blues")# <matplotlib.colors.LinearSegmentedColormap object at 0x7fa6149b5da0>
