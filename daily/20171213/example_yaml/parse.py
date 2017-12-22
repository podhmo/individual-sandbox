import yaml

with open("conflict.yaml") as rf:
    d = yaml.load(rf)
print(d)
