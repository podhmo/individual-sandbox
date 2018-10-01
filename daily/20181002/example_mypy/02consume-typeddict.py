import typing as t
import mypy_extensions as mx


class Person(mx.TypedDict):
    name: str
    age: int


class PersonWithNickname(Person, total=False):
    nickname: str


class Consumer:
    def __init__(self, typ, *, strict=True):
        self.hints = t.get_type_hints(typ)
        self.count = {k: 0 for k in self.hints}
        self.strict = strict

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        if typ is not None:
            return

        # todo: support total=False
        duplicated, unused = {}, {}
        for k, c in self.count.items():
            if c == 1:
                continue
            if c <= 0:
                unused[k] = c
            elif c > 1:
                duplicated[k] = c

        if unused or duplicated:
            raise ValueError(
                f"unexpected consuming -- duplicated={duplicated!r}, unused={unused!r}"
            )

    def __getitem__(self, name):
        if self.strict and name not in self.hints:
            raise ValueError(f"missing key {name!r}")
        self.count[name] += 1
        return name


with Consumer(Person) as c:
    print(f"select {c['name']}, {c['age']} from people")

# with Consumer(PersonWithNickname) as c:
#     print(f"select {c['name']}, {c['age']} from people")


# compiled function
def fetch(db) -> t.Cursor[Person]:
    pass
