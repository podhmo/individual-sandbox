import json


def construct_dict(pairs):
    print(pairs)
    assert len(pairs) == set(pair[0] for pair in pairs)
    return dict(pairs)


with open("conflict.json") as rf:
    d = json.load(rf, object_pairs_hook=construct_dict)

print(d)
