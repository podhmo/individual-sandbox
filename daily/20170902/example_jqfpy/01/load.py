import yaml

with open("xxx.yaml") as rf:
    loader = yaml.Loader(rf)
    print(loader.get_single_data())
