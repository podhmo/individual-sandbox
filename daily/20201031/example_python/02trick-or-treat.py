class Trick:
    def __bool__(self) -> bool:
        print("trick")
        return True


class Treat:
    def __bool__(self) -> bool:
        print("treat")
        return True


if Trick() or Treat():
    pass
if Trick() and Treat():
    pass
