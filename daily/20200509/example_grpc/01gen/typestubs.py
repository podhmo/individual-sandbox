import typing as t


def proto_package(path: str) -> t.Callable[[t.Type[t.Any]], t.Type[t.Any]]:
    def _deco(cls: t.Type[t.Any]) -> t.Type[t.Any]:
        cls.PROTO_PACKAGE = path
        return cls

    return _deco


#
# uint32,uint64などの違いがある？
uint64 = t.NewType("uint64", int)
uint32 = t.NewType("uint32", int)
uint = uint64


@proto_package("google/type/date.proto")
class Date:
    pass


@proto_package("google/protobuf/empty.proto")
class Empty:
    pass
