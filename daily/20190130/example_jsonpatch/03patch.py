import jsonpatch

src = {"name": "foo", "age": 20}
patches = [
    {
        "op": "path",
        "copy": "/person",
        "from": "/name"
    },
]

print(jsonpatch.JsonPatch(patches).apply(src))
