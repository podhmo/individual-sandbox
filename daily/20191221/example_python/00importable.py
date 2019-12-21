import os.path
import sys
from importlib.util import find_spec


def _is_importable(name: str) -> bool:  # xxx: danger

    cwd = os.path.normpath(os.path.abspath(os.getcwd()))
    i = None
    if cwd in sys.path:
        i = sys.path.index(cwd)
        sys.path.pop(i)
    try:
        module = sys.modules.pop(name, None)
        try:
            module_spec = find_spec(name)
            return module_spec is not None
        except Exception:
            raise
        finally:
            if module is not None:
                sys.modules[name] = module
    except Exception:
        raise
    finally:
        if i is not None:
            sys.path.insert(i, cwd)


import re  # noqa F402

print(_is_importable("re"))
print(_is_importable("re2"))
import re as re2  # noqa F402

print(re == re2)
