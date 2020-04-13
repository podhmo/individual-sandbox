from dictknife.transform import flatten
from dictknife.accessing import Accessor
from dictknife.langhelpers import as_path_node


def unflatten(d, *, sep="/", accessor=Accessor()):
    r = accessor.make_dict()
    for k, v in d.items():
        accessor.assign(r, [as_path_node(x) for x in k.split(sep)], v)
    return r


iterable = [(1, 2, 3), (4, (5, 6))]

print(flatten(iterable))
print(unflatten(flatten(iterable)))

print("----------------------------------------")

d = {"paths": {"/foo/bar": "xxxx"}}
print(flatten(d))
print(unflatten(flatten(d)))
