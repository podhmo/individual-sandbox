import typing as t
from typing_extensions import Protocol
from collections import defaultdict
from dataclasses import dataclass, asdict


class GetKey(Protocol):
    def get_key(self) -> str:
        ...


class Materializable(Protocol):
    def materialize(self) -> t.Tuple[GetKey, dict]:
        ...


class Loader(Protocol):
    def load(self) -> t.Any:
        ...

    def dump(self, ob: list) -> None:
        ...


class Backend:
    def __init__(self, loader: Loader):
        self.resources = []
        self.keys = defaultdict(list)
        self.loader = loader

    def add(self, instance: GetKey):
        k = instance.get_key()

        if k in self.keys:
            if instance in self.keys[k]:
                raise ValueError(f"instance of {k} is already added")

        self.keys[k].append(instance)
        self.resources.append(instance)

    def save(self):
        self.loader.dump(self.resources)


class JSONLoader:
    def load(self) -> t.Any:
        pass

    def dump(self, resources: list):
        print("@", resources)
        import json

        print(json.dumps(resources, ensure_ascii=False, indent=2, default=str))


#########################################


class AWSObject:
    name: t.ClassVar[str] = ""
    type_: t.ClassVar[str] = ""

    def __init_subclass__(cls, *, name: str = "", type_=None):
        name = name or cls.__name__.lower()
        type_ = type_ or cls.mro()[1].name

        cls.name = name
        cls.type_ = type_
        print(f"# {cls=} {name=!r} {type_=!r}")

    @classmethod
    def get_key(cls) -> str:
        return f"{cls.type_}-{cls.name}"


class AWSInstance(AWSObject, name="aws_instance"):
    ami: str
    instance_type: str


########################################


class example(AWSInstance):
    ami = "ami-0c55b159cbfafe1f0"
    instance_type = "t2.micro"


def run():
    class example(AWSInstance):
        ami = "ami-0c55b159cbfafe1f0"
        instance_type = "t2.micro"

    b = Backend(loader=JSONLoader())
    for name, value in locals().items():
        if isinstance(value, type) and issubclass(value, AWSObject):
            print("@", value)
            b.add(value)
    b.save()


run()
