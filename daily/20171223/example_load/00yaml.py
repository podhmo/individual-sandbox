import yaml


def construct_dict(loader, node):
    pairs = loader.construct_pairs(node)
    assert len(pairs) == set(pair[0] for pair in pairs)
    return dict(pairs)


yaml.add_constructor('tag:yaml.org,2002:map', construct_dict)

with open("conflict.yaml") as rf:
    d = yaml.load(rf)
print(d)
