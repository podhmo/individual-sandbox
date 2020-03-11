from detector import show




d = {"name": "foo", "age": 20}
show(d)
d = {"name": "boo", "age": 0, "father": {"name": "foo", "age": 20}}
show(d)
d = {
    "name": "boo",
    "age": 0,
    "father": {"name": "foo", "age": 20},
    "mother": {"name": "moo", "age": 20},
}
show(d)
d = {
    "name": "boo",
    "age": 0,
    "parents": [{"name": "foo", "age": 20}, {"name": "moo", "age": 20},],
}
show(d)
