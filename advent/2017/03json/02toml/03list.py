import toml

L = [
    {
        "person": "foo",
        "age": 20
    },
    {
        "person": "bar",
        "age": 10
    },
]

with open("people.toml", "w") as wf:
    toml.dump(L, wf)
