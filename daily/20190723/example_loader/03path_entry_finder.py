import textwrap
import sys
from importlib.machinery import ModuleSpec

# todo: typed
class Wrapper:
    def __init__(self, internal):
        self.internal = internal

    def __getattr__(self, name):
        attr = getattr(self.internal, name)
        if not callable(attr):
            return attr

        def caller(*args, **kwargs):
            print(">>", self.internal.__name__, attr.__name__, args, kwargs)
            v = attr(*args, **kwargs)
            print("<<", self.internal.__name__, v)
            return v

        return caller

    def __call__(self, *args, **kwargs):
        print("!!", self.internal, args, kwargs)
        return self.internal(*args, **kwargs)


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


# PathFinder越しにつかうのはちょっとめんどくさい


def make_finder(path):
    # import traceback
    # traceback.print_stack()
    print("@@", path)
    return DummyImporter


# sys.meta_path[2] = Wrapper(sys.meta_path[2])  # wrap PathFinder
sys.path.append(".")
sys.path_hooks.insert(0, make_finder)
# print(sys.path_importer_cache)
import xxx  # noqa

print(xxx.hello())
import yyy  # noqao

print(yyy.hello())
