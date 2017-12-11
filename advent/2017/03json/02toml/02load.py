import toml

with open("table.toml") as rf:
    d = toml.load(rf)

print(d)
