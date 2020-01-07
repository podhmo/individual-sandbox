import typing as t


def get_metadata(typ: t.Type[t.Any]) -> t.Dict[str, t.Any]:
    return getattr(typ, "__metadata__", None) or {}


def with_metadata(typ: t.Type[t.Any], **params: t.Any) -> t.Type[t.Any]:
    if not hasattr(typ, "__metadata__"):
        typ.__metadata__ = {}
    typ.__metadata__.update(params)
    return typ


UserId = with_metadata(t.NewType("UserId", int), hmm="oyoyo")

print(UserId.__name__)
print(get_metadata(UserId))
