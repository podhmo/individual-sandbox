import json
from json.decoder import WHITESPACE

data = """
{
  "name": "foo",
  "age": 20
}
{
  "name": "bar",
  "age": 10
}
{
  "name": "boo",
  "age": 22
}
"""


def parse_iter(s):
    size = len(s)
    decoder = json.JSONDecoder()

    end = 0
    while True:
        idx = WHITESPACE.match(data).end()
        if end + idx >= size:
            break
        ob, end = decoder.raw_decode(data, end + idx)
        yield ob


print(list(parse_iter(data)))
