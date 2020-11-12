import pathlib
import sys
from importlib.machinery import ModuleSpec


class OnefileImporter:  # importer = finder + loader
    def __init__(self, filename):
        self.filename = filename
        self._code_map = None

    @property
    def code_map(self):
        if self._code_map is not None:
            return self._code_map
        with open(self.filename) as rf:
            import ast

            self._code_map = ast.literal_eval(rf.read())
        return self._code_map

    def find_spec(self, fullname, path=None, target=None):
        code = self.code_map.get(fullname)
        if code is None:
            return None
        loader = self
        spec = ModuleSpec(fullname, loader)
        if not code:
            spec.submodule_search_locations = [str(pathlib.Path(__file__).parent)]
        return spec

    def create_module(self, spec):
        return None

    def exec_module(self, module):
        code = self.code_map[module.__name__]
        exec(code, module.__dict__)


sys.meta_path.append(OnefileImporter(pathlib.Path(__file__).parent / "__embed__.py"))

# import xxx.yyy.zzz

import main  # noqa
