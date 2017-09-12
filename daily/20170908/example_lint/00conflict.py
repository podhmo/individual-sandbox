import json


def f():
    json = {"x": 10}
    return json.dumps(json)


json.dumps(f())
