import dataclasses


@dataclasses.dataclass(frozen=True)
class Author:
    name: str = "<author>"


@dataclasses.dataclass(frozen=True)
class Message:
    name: str = "message"
    author: Author = Author()


print(Message())
