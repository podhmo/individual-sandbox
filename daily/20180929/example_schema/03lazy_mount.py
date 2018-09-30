from dictknife import loading
from schema import Namespace, Object, Array


class Person(Object):
    """person"""

    name: str
    age: int


class People(Array):
    items = Person


schemas = Namespace("schemas")
schemas.mount(People)

with Namespace("components") as components:
    components.mount(schemas)
    loading.dumpfile(components.as_dict(), format="json")
