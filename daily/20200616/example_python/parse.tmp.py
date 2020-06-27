import importlib
from importlib.machinery import SourceFileLoader
import sys

# from moz_sql_parser.sql_parser

pre_set = set(sys.modules.keys())

spec = importlib.util.find_spec("moz_sql_parser.sql_parser")
assert isinstance(spec.loader, SourceFileLoader), spec.loader.__class__


class SourceFileLoaderWithTransform(SourceFileLoader):
    def get_source(self, fullname: str) -> str:
        return self.transform_source(super().get_source(fullname))

    def transform_source(self, source: str) -> str:
        return source


spec.loader.__class__ = SourceFileLoaderWithTransform
spec.loader.load_module(spec.name)
print(set(sys.modules.keys()).difference(pre_set))
