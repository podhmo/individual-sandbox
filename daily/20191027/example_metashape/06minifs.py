import typing as t
from dictknife import DictWalker
from dictknife.transform import flatten

T = t.TypeVar("T")


class MiniFS(t.Generic[T]):
    fs: t.Dict[str, t.Any]  # recursive
    sep: str

    def __init__(self, *, sep="/", store: t.Optional[t.Dict[str, t.Any]] = None):
        self._store = store or {}
        self.sep = sep

    def glob(self, query: str) -> t.Dict[str, T]:
        pass

    def create_file(self, name: str) -> T:
        pass

    def open_file(self, name: str) -> t.Optional[T]:
        pass


def _make_walker_from_glob(query: str, *, sep="/") -> DictWalker:
    toks = query.split(sep)


def walk(fs: MiniFS[T]) -> t.Iterable[T]:
    for row in flatten(fs._store, sep=fs.sep).items():
        yield row


def main():
    fs = MiniFS(
        store={
            "projects": {
                "x.txt": "xxx",
                "y.txt": "yyy",
                "sub_projects": {"z0.txt": "zzz", "z1.txt": "zzz"},
            }
        }
    )
    for fname, content in walk(fs):
        print(fname, content)


main()
