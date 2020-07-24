from __future__ import annotations
import dataclasses
from typing_extensions import Annotated
from metashape.declarative import ignore, field


@ignore
@dataclasses.dataclass
class Description:
    description: str


class XXXServices:
    @ignore
    @dataclasses.dataclass
    class Service:
        name: str
        outdated: bool = False

    foo: Annotated[Service, Description("foo field")] = field(
        default=Service(name="foo")
    )
    bar: Service = Service(name="bar")
    boo: Service = field(default=Service(name="boo"))


if __name__ == "__main__":
    from metashape.runtime import get_walker

    w = get_walker(aggressive=True)
    for cls in w.walk():
        print(cls)
        fw = w.for_type(cls)
        for f in fw.walk():
            print("\t", f)
