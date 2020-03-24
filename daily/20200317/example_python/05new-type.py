from typing import NewType

Foo = NewType("Foo", str)


def use_foo_like_str(foo: Foo):
    # Fooはstrではないのでstrのメソッドが使えない
    print(foo.replace("xxxx", "yyy"))


def use_foo_as_str(foo: Foo):
    def _use(x: str):
        print(x.replace("xxxx", "yyy"))

    # Fooはstrではないのでstrになれない
    _use(foo)


def main() -> None:
    foo = Foo("foo")
    reveal_type(foo) 
    use_foo_like_str(foo)

    foo2 :Foo = "foo"
    reveal_type(foo2) 
    use_foo_like_str(foo2)

    x = "foo"
    reveal_type(x) 
    use_foo_like_str(x)
