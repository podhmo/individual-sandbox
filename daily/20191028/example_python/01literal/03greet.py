import typing_extensions as tx

Action = tx.Literal["hello", "bye"]


def greet(name: str, prefix: Action = "hello") -> str:
    return f"{prefix} {name}!"


greet("hell", prefix="Go to")
