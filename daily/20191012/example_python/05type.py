import typing as t

if t.TYPE_CHECKING:
    reveal_type(set([int, str]))

    ts: t.Set[t.Type[t.Any]] = set([int, str])
    reveal_type(ts)

    ts2 = set([int, str])  # type: t.Set[t.Type[t.Any]]
    reveal_type(ts)
