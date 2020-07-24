import dataclasses


@dataclasses.dataclass
class Service:
    name: str
    outdated: bool = False


class XXXServices:
    foo: Service(name="foo")
    bar: Service(name="bar")
    boo: Service(name="boo")


if __name__ == "__main__":
    from metashape.runtime import get_walker

    for cls in get_walker(aggressive=True).walk():
        print(cls)
