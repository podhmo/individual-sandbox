import typing as t
G = t.TypeVar("G")


class T:
    def __init__(self, restriction: t.Dict) -> None:
        self.restriction = restriction

    def __str__(self) -> str:
        return "t"

    def __getattr__(self, name: str) -> str:
        if name not in self.restriction:
            raise RuntimeError(f"unexpected field found: {name!r}")
        return name


class Person:
    name: str
    age: int


# mapped type
def fetchall(db: object, t: G, tmpl: str, *args: t.Any) -> t.List[G]:
    print(tmpl)
    return []


def main() -> None:
    db = object()
    t = T({"name": str, "age": int})

    for row in fetchall(db, Person, f"""SELECT {t.name}, {t.age} from {t}"""):
        print(row.name, row.age, row.foo)

    # error
    # fetchall(db, f"""SELECT {t.name}, {t.age}, {t.missing} from {t}""")


if __name__ == "__main__":
    main()
