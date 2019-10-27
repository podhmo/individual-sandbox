import typing_extensions as tx

Direction = tx.Literal["u", "d", "r", "l"]


def on_u(d: Direction) -> None:
    print("U")


def main() -> None:
    d: Direction = "u"
    if d == "U":  # not "u"
        on_u(d)
    else:
        print("other")
