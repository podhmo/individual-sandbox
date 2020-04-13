def hello(*, name: str, age: int = 0) -> None:
    """HelloCommand"""
    print(f"Hello {name}!!")


def byebye(*, name: str) -> None:
    """ByebyeCommand"""
    print(f"Byebye {name}!!")
