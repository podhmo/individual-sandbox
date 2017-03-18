from collections import ChainMap


top = {"top": "hai"}
middle = {"middle": "hoi"}
bottom = {"bottom": "yay"}

# untyped
stacked = ChainMap(bottom, ChainMap(middle, top))
print(stacked["top"])
print(stacked["middle"])
print(stacked["bottom"])
