import typing_extensions as tx
import mypy_extensions as mx


class HasA(tx.Protocol):
    a: str


class HasB(tx.Protocol):
    b: str


class AB(HasA, HasB):
    pass


def use(ob: AB) -> None:
    print(ob.a, ob.b)


Params = mx.TypedDict("Params", {"a": str, "b": str})


def run(d: Params) -> None:
    class Ob:
        z = "boo"

    class Ob2(Ob, AB):
        a = d["a"]
        b = d["b"]

    ob = Ob2()
    use(ob)
    print(ob.z)


def main() -> None:
    run({"a": "foo", "b": "bar"})


if __name__ == "__main__":
    main()
