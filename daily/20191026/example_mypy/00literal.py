import typing_extensions as tx

Direction = tx.Literal["u", "d", "r", "l"]


def main() -> None:
    d: Direction = "u"
    if d == "U":  # not "u"
        print("U")
    else:
        print("other")
