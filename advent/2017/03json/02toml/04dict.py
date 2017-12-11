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

d = {"people": L}

with open("people.toml", "w") as wf:
    toml.dump(d, wf)
