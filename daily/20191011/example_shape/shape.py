import sys
import typing as t
import typing_extensions as tx
import json

# util
if (sys.version_info[0], sys.version_info[1]) >= (3, 7):
    make_dict = dict
else:
    from collections import OrderedDict as make_dict  # noqa

# to jsonschema
# to openapi
# to graphql
# to grpc
# to python code
# to db tables


T = t.TypeVar("T")


# resolve
def mark(cls: t.Type[T]) -> t.Type[T]:
    if is_marked(cls):
        return cls
    setattr(cls, "_shape_mark", True)
    return cls


def is_marked(cls: t.Type[T]) -> bool:
    return hasattr(cls, "_shape_mark")


class Member(tx.Protocol):
    _shape_mark: bool


# renderer
class Strategy(tx.Protocol):
    def render(self, ob: t.Type[T]) -> None:
        ...

    def resolve(self, ob: t.Type[T]) -> None:
        ...


class Repository(tx.Protocol):
    @property
    def members(self) -> t.List[Member]:  # support also instance variable...
        ...


RepositoryT = t.TypeVar("RepositoryT", bound="Repository")


class Resolver(tx.Protocol):
    def resolve_name(self, m: Member) -> str:
        ...


class FakeResolver(Resolver):
    def resolve_name(self, member: Member) -> str:
        return member.__name__  # type: ignore


class FakeRepository(Repository):
    def __init__(self, members: t.List[t.Any]) -> None:
        self._members = t.cast(t.List[Member], members)  # xxx

    @property
    def members(self) -> t.List[Member]:
        return self._members

    @classmethod
    def from_dict(
        cls, d: t.Dict[str, t.Any]
    ) -> "FakeRepository":  # todo: subtyping support
        members = [v for v in d.values() if is_marked(v)]
        return cls(members)


class OpenAPI:
    output: t.Dict[str, t.Any]  # xxx

    def __init__(self, resolver: Resolver, repository: Repository) -> None:
        self.resolver = resolver
        self.repository = repository
        self.output = make_dict()  # xxx
        self.output["components"] = make_dict()
        self.output["components"]["members"] = make_dict()

    def emit(self, member: Member) -> None:
        name = self.resolver.resolve_name(member)
        self.output["components"]["members"][name] = {}


def render(repository: Repository) -> str:
    emitter = OpenAPI(FakeResolver(), repository)
    for m in repository.members:
        emitter.emit(m)
    return json.dumps(emitter.output, indent=2, ensure_ascii=False)
