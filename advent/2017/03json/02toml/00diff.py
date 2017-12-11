import toml

d = {
    "/": {
        "200": {
            "description": "ok response"
        },
        "default": {
            "description": "default response"
        },
    },
}

with open("schema.toml", "w") as wf:
    toml.dump(d, wf)
with open("schema.toml", "r") as rf:
    from_toml = toml.load(rf)
