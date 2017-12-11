import yaml

with open("list.yaml") as rf:
    L = yaml.load(rf)


print(L)
# [{'age': 20, 'person': 'foo'}]
