from metashape.runtime import get_walker

class Person:
    name: str

w = get_walker(aggressive=True)
for c in w.walk():
    print(c)
