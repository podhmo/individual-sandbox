from dictknife import deepmerge

d = {
    "xs": {
        "a": {
            "name": "a",
            "vs": [{"v": 1}, {"v": 2}, {"v": 3}]
        }
    },
}

d2 = {
    "xs": {
        "a": {
            "name": "a'",
            "vs": [{"v": 10}]
        }
    },
}

print(deepmerge(d, d2, make_dict=dict))
# {'xs': {'a': {'vs': [{'v': 1}, {'v': 2}, {'v': 3}, {'v': 10}], 'name': "a'"}}}
print(deepmerge(d, d2, override=True, make_dict=dict))
# {'xs': {'a': {'vs': [{'v': 10}], 'name': "a'"}}}
