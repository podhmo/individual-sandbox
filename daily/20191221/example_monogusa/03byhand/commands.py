def hello(*, name: str = "world") -> None:
    print(f"hello {name}")


def byebye(*, name: str) -> None:
    print(f"byebye {name}")
