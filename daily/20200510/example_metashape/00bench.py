import timeit

setup = """
from metashape.analyze.typeinfo import typeinfo
import typing as t
class Person:
    pass
MAP = t.Dict[Person, t.List[Person]]
info = typeinfo(MAP)
"""
code = """
s = set()
for i in range(1000):
    s.add(info)
"""

print(timeit.timeit(setup=setup, stmt=code, number=100))
