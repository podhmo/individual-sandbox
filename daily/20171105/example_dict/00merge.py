from dictknife import deepmerge

d = {
    "xs": {
        "a": {
            "name": "a",
            "vs": [1, 2, 3]
        }
    },
}

d2 = {
    "xs": {
        "a": {
            "name": "a'",
            "vs": [10]
        }
    },
}

print(deepmerge(d, d2, make_dict=dict))
# {'xs': {'a': {'vs': [1, 2, 3, 10], 'name': "a'"}}}
print(deepmerge(d, d2, override=True, make_dict=dict))
# {'xs': {'a': {'vs': [10], 'name': "a'"}}}
