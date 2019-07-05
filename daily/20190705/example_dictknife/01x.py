from dictknife import deepmerge

d = {"db": {"name": "foo", "tables": [{"name": "b"}]}}
d2 = {"db": {"name": "foo@", "tables": [{"name": "a"}]}}
print(deepmerge(d, d2, method="replace"))
