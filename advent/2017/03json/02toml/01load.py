import toml

with open("data.toml") as rf:
    d = toml.load(rf)

print(d)
