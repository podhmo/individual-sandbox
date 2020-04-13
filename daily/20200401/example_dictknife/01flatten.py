from dictknife.transform import flatten, unflatten


iterable = [(1, 2, 3), (4, (5, 6))]

print(flatten(iterable))
print(unflatten(flatten(iterable)))

print("----------------------------------------")

d = {"paths": {"/foo/bar": "xxxx"}}
print(flatten(d))
print(unflatten(flatten(d)))
