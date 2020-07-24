import dataclasses
from metashape.declarative import ignore, field


@ignore
@dataclasses.dataclass
class Service:
    name: str
    outdated: bool = False


class XXXServices:
    foo: Service = field(default=Service(name="foo"))
    bar: Service = field(default=Service(name="bar"))
    boo: Service = field(default=Service(name="boo"))


if __name__ == "__main__":
    from metashape.runtime import get_walker

    for cls in get_walker(aggressive=True).walk():
        print(cls)
