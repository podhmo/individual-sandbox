import json
from json.decoder import WHITESPACE


def loads_iter(s):
    size = len(s)
    decoder = json.JSONDecoder()

    end = 0
    while True:
        idx = WHITESPACE.match(s[end:]).end()
        i = end + idx
        if i >= size:
            break
        ob, end = decoder.raw_decode(s, i)
        yield ob

s = """
{"name": "foo"}
{
"name": "bar"
}
{
"name":
 "boo"
}
"""
print(list(loads_iter(s)))
