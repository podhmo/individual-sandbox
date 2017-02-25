import timeit


setup = """
from marshmallow import Schema, fields


class Person(Schema):
    name = fields.String()
    age = fields.Integer()
    father = fields.Nested("self")
    mother = fields.Nested("self")

s = Person(strict=True)
d = {'name': 'foo', 'age': 20, "mother": {"name": "boo", "age": 40}}
"""

N = 1000


print(timeit.repeat("s.dump(d)", setup=setup, number=N, repeat=3))
print(timeit.repeat("s.dump(d, update_fields=False)", setup=setup, number=N, repeat=3))
# [0.1354558959719725, 0.14255529499496333, 0.14005642299889587]
# [0.12603941999259405, 0.12192367800162174, 0.12901387197780423]
