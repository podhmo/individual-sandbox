class Person:
    name: str = "foo"
    age: int = 20


from metashape.runtime import get_walker
from prestring.naming import untitleize


d = {}
w = get_walker(aggressive=True)
for cls in w.walk():
    props = {}
    for name, _, _ in w.for_type(cls).walk():
        #  camelCase?
        props[name] = getattr(cls, name)

    typename = w.resolver.resolve_typename(cls)
    d[untitleize(typename)] = props

from dictknife import loading

loading.dumpfile(d, format="yaml")
