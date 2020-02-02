import typing as t
import typing_extensions as tx
import pickle

K = t.TypeVar("K")
V = t.TypeVar("V")
T_co = t.TypeVar("T_co", covariant=True)


class Loading(tx.Protocol[K, T_co]):
    def encode(self, ob: t.Any) -> K:
        ...

    def decode(self, b: K) -> T_co:
        ...


class PickleLoading(t.Generic[V]):
    def encode(self, ob: V) -> bytes:
        return pickle.dumps(ob)

    def decode(self, b: bytes) -> V:
        return t.cast(V, pickle.loads(b))


class Identity(t.Generic[V]):
    def encode(self, ob: V) -> V:
        return ob

    def decode(self, b: V) -> V:
        return b


class Q(t.Generic[K, V]):
    xs: t.List[K]

    def __init__(self, loading: Loading[K, V]) -> None:
        self.xs: t.List[K] = []
        self.loading = loading

    def put(self, v: V) -> None:
        b = self.loading.encode(v)
        print(f"-> {v} as {b}")
        self.xs.append(b)

    def get(self) -> V:
        b = self.xs.pop(0)
        v = self.loading.decode(b)
        print(f"<- {v} as {b}")
        return v


def use(loading: Loading[K, V], ob: V) -> V:
    k = loading.encode(ob)
    print(f"encoded: {k!r}")
    v = loading.decode(k)
    print(f"decoded: {v!r}")
    return v


loading = PickleLoading[str]()
print(loading.decode(loading.encode("foo")))
print(use(loading, "foo"))
identity = Identity[str]()
print(use(identity, "foo"))

q = Q(loading)
q.put("foo")
q.get()

q2 = Q(identity)
q2.put("foo")
q2.get()
