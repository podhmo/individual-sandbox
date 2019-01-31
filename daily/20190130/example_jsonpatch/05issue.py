import jsonpatch

doc = {"name": "foo"}
patches = [
    {
        "op": "copy",
        "path": "/person",
        "from": ""
    },
]
print(jsonpatch.JsonPatch(patches).apply(doc))
