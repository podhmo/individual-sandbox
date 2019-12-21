def hello(*, name: str = "world") -> None:
    return f"hello {name}"


def byebye(*, name: str) -> None:
    return f"byebye {name}"
