import typing as t
from functools import partial
from collections import defaultdict

# todo: nullable
# todo: hetero list
# todo: many=True
# todo: zero

uid = str
TypeInfo = t.Union["Object", "Container", "Primitive"]
Path = t.List[str]
JSONType = t.Union[int, float, str, bool, t.List["JSONType"], t.Dict[str, "JSONType"]]


class Object:
    def __init__(
        self, sig: t.Any, *, path: Path, props: t.Dict, raw: t.Dict[t.Any, t.Any]
    ) -> None:
        self.sig = sig
        self.path = path
        self.props = props
        self._raw = raw
        self._others: t.List[t.Tuple[Path], t.Dict[t.Any, t.Any]] = []

    def clone(self) -> "Object":
        new = self.__class__(self.sig, path=self.path, props=self.props, raw=self._raw)
        new._others = self._others
        return new

    @property
    def size(self):
        return len(self.props)

    def __repr__(self):
        return f"<Object size={self.size} path={self.path!r}>"


class Container:
    def __init__(
        self,
        sig: t.Any,
        *,
        path: Path,
        base: t.Type[t.Any],
        item: TypeInfo,
        raw: t.Dict[t.Any, t.Any],
    ) -> None:
        self.sig = sig
        self.path = path
        self.base = base
        self.item = item
        self._raw = raw
        self._others: t.List[t.Tuple[Path], t.Dict[t.Any, t.Any]] = []

    @property
    def size(self):
        return len(self._raw)

    def clone(self) -> "Container":
        new = self.__class__(
            self.sig, path=self.path, base=self.base, item=self.item, raw=self._raw
        )
        new._others = self._others
        return new

    def __repr__(self):
        return f"<Container base={self.base} size={self.size} path={self.path!r}>"


ListC = partial(Container, base=t.List)
OptionalC = partial(Container, base=t.Optional)


class Primitive:
    def __init__(
        self,
        sig: t.Any,
        *,
        path: Path,
        type_: t.Type[t.Any],
        raw: t.Dict[t.Any, t.Any],
    ) -> None:
        self.sig = sig
        self.path = path
        self.type = type_
        self._raw = raw
        self._others: t.List[t.Tuple[Path], t.Dict[t.Any, t.Any]] = []

    @property
    def size(self):
        return 0

    def clone(self) -> "Primitive":
        new = self.__class__(self.sig, path=self.path, type_=self.type, raw=self._raw)
        new._others = self._others
        return new

    def __repr__(self):
        return f"<Primitive type={self.type!r} path={self.path!r}>"


SENTINEL = object()
ZERO = Object(tuple(), path=":zero:", props={}, raw={})


class Result:
    def __init__(self):
        self.registry: t.Dict[uid, TypeInfo] = {}
        self.history: t.List[TypeInfo] = []
        self._uid_map: t.Dict[t.Any, uid] = defaultdict(lambda: len(self._uid_map))

    def __repr__(self):
        return f"<Result size={len(self.history)}>"

    def add(self, uid: uid, info: TypeInfo) -> TypeInfo:
        self.registry[uid] = info
        self.history.append(info)  # xxx
        return info

    def __contains__(self, uid: uid):
        return uid in self.registry

    def get(self, uid: uid, *, default):
        return self.registry.get(uid, default)


class Detector:
    # todo: many
    def detect(self, d: JSONType, *, path: Path, result: Result) -> TypeInfo:
        if isinstance(d, dict):
            return self.detect_dict(d, path=path, result=result)
        elif isinstance(d, (list, tuple)):
            return self.detect_list(d, path=path, result=result)
        else:
            return self.detect_primitive(d, path=path, result=result)

    def detect_dict_many(
        self,
        xs: t.Collection[t.Dict[JSONType, JSONType]],
        *,
        path: Path,
        result: Result,
    ) -> TypeInfo:
        seen = set()

        modified = False
        ma = ZERO

        # TODO: update signature?
        # TODO: union
        if len(xs) > 0:
            ma = self.detect_dict(xs[0], path=path, result=result)
            seen.add(id(ma))

        for x in xs[1:]:
            info = self.detect_dict(x, path=path, result=result)

            if id(info) in seen:
                continue
            seen.add(id(info))

            if not modified:
                modified = True
                ma = ma.clone()

            if not isinstance(info, Object):
                raise RuntimeError(f"{type(info)} is not Object")

            for k, val in info.props.items():
                ma_val = ma.props.get(k)

                if ma_val is None:
                    ma.props[k] = OptionalC(None, path=val.path, item=val, raw=val._raw)
                elif getattr(val, "base", None) is t.Optional:  # OptionalC
                    if getattr(ma_val, "base", None) is None:
                        assert ma_val == val.item
                        ma.props[k] = val
            for k, ma_val in ma.props.items():
                val = info.props.get(k)
                if val is None:
                    if getattr(ma_val, "base", None) is None:
                        ma.props[k] = OptionalC(
                            None, path=ma_val.path, item=ma_val, raw=ma_val._raw
                        )

        if modified:
            uid = result._uid_map[ma.sig]
            result.add(uid, ma)
        return ma

    def detect_dict(
        self, d: t.Dict[JSONType, JSONType], *, path: Path, result: Result
    ) -> TypeInfo:
        props = {}
        sigs = []

        for k, v in d.items():
            path.append(k)
            subinfo = props[k] = self.detect(v, path=path, result=result)
            sigs.append((k, subinfo.sig))
            path.pop()

        sig = frozenset(sigs)
        uid = result._uid_map[sig]
        info = result.get(uid, default=SENTINEL)

        if info is not SENTINEL:
            info._others.append((path[:], d))
            return result.registry[uid]

        return result.add(uid, Object(sig, path=path[:], raw=d, props=props))

    def detect_list(
        self, d: t.List[JSONType], *, path: Path, result: Result
    ) -> TypeInfo:
        path.append(":item:")
        inner_info = self.detect_dict_many(d, path=path, result=result)
        path.pop()
        return result.add(
            uid, ListC((list, inner_info.sig), item=inner_info, path=path[:], raw=d)
        )

    def detect_primitive(self, d: t.Any, *, path: Path, result: Result) -> TypeInfo:
        sig = type(d)
        uid = result._uid_map[sig]
        if uid in result:
            cached = result.registry[uid]
            if cached.path == path:
                return cached
            else:
                # todo: performance improvement
                new = cached.clone()
                new._others.append(d)
                new.path = path[:]
                return new

        return result.add(uid, Primitive(sig, path=path[:], raw=d, type_=type(d)))


def detect(d: JSONType, *, detector=Detector()):
    path: Path = []
    result = Result()
    detector.detect(d, path=path, result=result)
    return result


def show(d):
    print("-")
    r = detect(d)
    print(r)
    for info in r.history:
        print(" ", info)
