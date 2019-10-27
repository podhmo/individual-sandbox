import typing as t
from collections import namedtuple


class _Sentinel:
    def __init__(self, name: str) -> None:
        self.name = name

    def __str__(self):
        return "<{self.name}>"


# types
Key = t.Union[int, str]
Token = t.Union[_Sentinel, str, int]

# constants
STAR = _Sentinel("*")
DBSTAR = _Sentinel("**")


def parse(query: str, *, sep="/") -> t.Sequence[str]:
    r = []
    for tk in query.split(sep):  # todo: shlex?
        if tk == "**":
            r.append(DBSTAR)
        elif tk == "*":
            r.append(STAR)
        else:
            r.append(tk)
    return r


def dig(
    d: t.Dict[t.Any, t.Any],
    tokens: t.Sequence[t.Union[_Sentinel, str]],
    *,
    path: t.Optional[t.List[Key]] = None,
) -> t.Iterable[t.Tuple[t.List[Key], t.Any, bool]]:  # (path, value, ok)
    if not tokens:
        yield path, d, True
        return
    path = path or []
    rest_tks = tokens[1:]
    for path, val, ok in _dig_next(d, tokens[0], path):
        if ok:
            yield from dig(val, rest_tks, path=path)
        else:
            yield path, val, ok


def _dig_next(
    d: t.Dict[Key, t.Any], tk: Token, path: t.List[Key]
) -> t.Iterable[t.Tuple[t.List[Key], t.Any, bool]]:  # (path, value, ok)
    if tk == STAR:
        for k, v in d.items():
            path.append(k)
            yield path, v, True
            path.pop()
    else:
        if tk in d:
            path.append(tk)
            yield path, d[tk], True
            path.pop()
        else:
            yield path, d, False


def fix(rows):
    return [(path[:], val, ok) for path, val, ok in rows]


d = {"a": {"b": {"c": {"d": "ok"}}, "z": {"d": "zok"}}}

fix(dig(d, ["a"]))  # => [(['a'], {'b': {'c': {'d': 'ok'}}, 'z': {'d': 'zok'}}, True)]
fix(dig(d, ["a", "b"]))  # => [(['a', 'b'], {'c': {'d': 'ok'}}, True)]
fix(dig(d, ["a", "b", "c"]))  # => [(['a', 'b', 'c'], {'d': 'ok'}, True)]
fix(dig(d, ["a", "b", "c", "d"]))  # => [(['a', 'b', 'c', 'd'], 'ok', True)]
fix(dig(d, ["a", "b", "c", "e"]))  # => [(['a', 'b', 'c'], {'d': 'ok'}, False)]

fix(dig(d, ["a", "x"]))  # => [(['a'], {'b': {'c': {'d': 'ok'}}, 'z': {'d': 'zok'}}, False)]

fix(dig(d, ["a", "b", STAR, "d"]))  # => [(['a', 'b', 'c', 'd'], 'ok', True)]
fix(dig(d, ["a", STAR, "c"]))  # => [(['a', 'b', 'c'], {'d': 'ok'}, True), (['a', 'z'], {'d': 'zok'}, False)]
fix(dig(d, ["a", STAR, "d"]))  # => [(['a', 'b'], {'c': {'d': 'ok'}}, False), (['a', 'z', 'd'], 'zok', True)]
fix(dig(d, ["a", STAR, STAR]))  # => [(['a', 'b', 'c'], {'d': 'ok'}, True), (['a', 'z', 'd'], 'zok', True)]
