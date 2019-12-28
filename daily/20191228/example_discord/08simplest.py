def hello(*, name: str = "world") -> None:
    """hello message"""
    print(f"hello {name}")


def ng(*, name: str = "world") -> None:
    """NG"""
    1 / 0


def byebye(*, name: str = "world") -> None:
    """byebye :wave:"""
    print(f"byebye {name}")
