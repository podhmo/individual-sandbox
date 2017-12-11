import yaml
with open("data.json", "r") as rf:
    d = yaml.load(rf)
print(d)
