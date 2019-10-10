import typing as t
import typing_extensions as tx
import sys
import json
import logging


logger = logging.getLogger(__name__)
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
    def is_member(self, ob: t.Type[T]) -> bool:
        ...

    def resolve_name(self, m: Member) -> str:
        ...


class FakeResolver(Resolver):
    def __init__(
        self, *, is_member: t.Optional[t.Callable[[t.Type[T]], bool]] = None
    ) -> None:
        self._is_member = is_member or is_marked

    def is_member(self, ob: t.Type[T]) -> bool:
        return self._is_member(ob)

    def resolve_name(self, member: Member) -> str:
        return member.__name__  # type: ignore

    def resolve_repository(self, d: t.Dict[str, t.Any]) -> "FakeRepository":
        members = [v for v in d.values() if self.is_member(v)]
        return FakeRepository(members)


class FakeRepository(Repository):
    def __init__(self, members: t.List[t.Any]) -> None:
        self._members = t.cast(t.List[Member], members)  # xxx

    @property
    def members(self) -> t.List[Member]:
        return self._members


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


def render(repository: Repository, resolver: t.Optional[Resolver] = None) -> str:
    resolver = resolver or FakeResolver()
    emitter = OpenAPI(resolver, repository)
    logger.debug("collect members: %d", len(repository.members))
    for m in repository.members:
        emitter.emit(m)
    return json.dumps(emitter.output, indent=2, ensure_ascii=False)


def run(
    filename: str,
    *,
    aggressive: bool = False,
    is_member: t.Optional[t.Callable[[t.Type[T]], bool]] = None
) -> None:
    from magicalimport import import_module  # type:ignore

    m = import_module(filename)
    if aggressive:
        is_member = lambda x: hasattr(x, "__name__")  # noqa
    resolver = FakeResolver(is_member=is_member)
    repository = resolver.resolve_repository(m.__dict__)
    print(render(repository, resolver=resolver))


def main(*, argv: t.Optional[t.List[str]] = None) -> None:
    import argparse
    import logging

    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help  # type:ignore
    parser.add_argument("filename")
    parser.add_argument("--aggressive", action="store_true")
    parser.add_argument(
        "--logging", choices=list(logging._nameToLevel.keys()), default="DEBUG"
    )
    args = parser.parse_args(argv)

    params = vars(args)
    logging.basicConfig(level=params.pop("logging"))
    run(**params)


if __name__ == "__main__":
    main()
