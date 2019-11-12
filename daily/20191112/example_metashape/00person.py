from metashape.runtime import get_walker


class Person:
    name: str
    age: int


w = get_walker(aggressive=True)
for cls in w.walk():
    print(cls)
