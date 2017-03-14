S = [set(), {"A"}, {"B"}, {"C"}, {"A", "B"}, {"A", "C"}, {"B", "C"}, {"A", "B", "C"}]


def classify(e, xs):
    d = {"subset": [], "superset": []}
    for x in xs:
        if x.issubset(e):
            d["subset"].append(x)
        if x.issuperset(e):
            d["superset"].append(x)
    return d


for x in S:
    d = classify(x, S)
    print(x)
    print("    ", "superset:", d["superset"])
    print("    ", "subset:", d["subset"])

"""
{'A'}
     superset: [{'A'}, {'B', 'A'}, {'C', 'A'}, {'C', 'B', 'A'}]
     subset: [set(), {'A'}]


{'B', 'A'}
     superset: [{'B', 'A'}, {'C', 'B', 'A'}]
     subset: [set(), {'A'}, {'B'}, {'B', 'A'}]


{'C', 'B', 'A'}
     superset: [{'C', 'B', 'A'}]
     subset: [set(), {'A'}, {'B'}, {'C'}, {'B', 'A'}, {'C', 'A'}, {'C', 'B'}, {'C', 'B', 'A'}]
"""
