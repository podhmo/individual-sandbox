from metashape.runtime import get_walker
from metashape.outputs.openapi.emit import scan, Context
from dictknife import loading


class Person:
    name: str


def app(c: Context) -> None:
    c.result.store["version"] = "3.0.0"


w = get_walker(aggressive=True)
c = scan(w)

# print(c.status)
# print(c.result)

app(c)

loading.dumpfile(c.result.store)
