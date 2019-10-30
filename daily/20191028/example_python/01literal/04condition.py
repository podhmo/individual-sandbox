import typing_extensions as tx

Direction = tx.Literal["up", "down", "left", "right"]


def use(d: Direction) -> None:
    if d == "UP":  # not "up"
        return print("UUUUUUUUUUUUUUUUUUUU")
    else:
        return print("ELSE")
