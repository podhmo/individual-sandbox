import jsonpatch

doc = {"x": {"name": "foo"}}
patches = [
    {
        "op": "add",
        "path": "",
        "from": "x"
    },
]
print(jsonpatch.JsonPatch(patches).apply(doc))
