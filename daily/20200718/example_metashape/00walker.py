from metashape import runtime


class Person:
    name: str
    age: int


w = runtime.get_walker([Person])
for cls in w.walk():
    print(cls)
    for name, typeinfo, metadata in w.for_type(cls).walk():
        print("\t", name, typeinfo.raw, metadata)
