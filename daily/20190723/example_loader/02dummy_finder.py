import textwrap
import sys
from importlib.machinery import ModuleSpec

# todo: typed


class DummyImporter:  # importer = finder + loader
    @classmethod
    def find_spec(cls, fullname, path=None, target=None):
        loader = cls
        return ModuleSpec(fullname, loader)

    @classmethod
    def create_module(cls, spec):
        return None

    @classmethod
    def exec_module(cls, module):
        code = textwrap.dedent(
            """
        def hello():
            return f"hello from {__name__}"
        """
        )
        exec(code, module.__dict__)


sys.meta_path.append(DummyImporter)

# execしても良いけれど既存のものをそのまま使いたいなー。
import xxx  # noqa

print(xxx.hello())
import yyy  # noqa

print(yyy.hello())
