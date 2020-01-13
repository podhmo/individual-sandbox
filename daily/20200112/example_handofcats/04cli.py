def hello(*, name: str = "world"):
    print(f"hello {name}")


def byebye(*, name: str):
    print(f"byebye {name}")


# ignored
def _ignore(name: str):
    print("ignored")
