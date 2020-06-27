import sys
import re
from importlib.machinery import SourceFileLoader


class _SourceFileLoaderWithTransform(SourceFileLoader):
    def transform(self, code: bytes, *, rx=re.compile(r"\nIDENT_CHAR = .*")) -> bytes:
        source = code.decode("utf-8")
        source = rx.sub(lambda m: m.group(0) + "+ ':'", source, 1)
        return source.encode("utf-8")

    def get_data(self, path) -> bytes:
        if path.endswith(".pyc"):
            path = path[:-1]
        code = super().get_data(path)
        return self.transform(code)


class _InterceptFinder:
    @classmethod
    def find_spec(cls, fullname, path=None, target=None):
        if fullname != "moz_sql_parser.sql_parser":
            return None

        for finder in sys.meta_path:
            if finder == cls:
                continue
            spec = finder.find_spec(fullname, path=path, target=target)
            if spec is not None:
                break
        assert isinstance(spec.loader, SourceFileLoader), type(spec.loader)
        spec.loader.__class__ = _SourceFileLoaderWithTransform
        return spec


# black magic
assert "moz_sql_parser" not in sys.modules
sys.meta_path.insert(0, _InterceptFinder)
from moz_sql_parser import parse  # noqa F401

sys.meta_path.remove(_InterceptFinder)
__all__ = ["parse"]
