import something

d = {"name": "foo", "age": 20}
pair = [d, d]  # shared
assert ["foo", "foo"] == [x["name"] for x in pair]

# after modified
pair[0]["name"] = "bar"
assert ["bar", "bar"] == [x["name"] for x in pair]

pair2 = something.loads(something.dumps(pair))
assert ["bar", "bar"] == [x["name"] for x in pair2]
pair2[0]["name"] = "foo"
assert ["foo", "foo"] == [x["name"] for x in pair]
