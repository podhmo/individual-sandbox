from handofcats.driver import Driver


def hello(*, name: str) -> None:
    print(f"hello {name}")


d = Driver()
d.run(hello)
