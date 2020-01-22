import os.path
from jedi.inference.gradual import typeshed
import jedi
from parso.utils import PythonVersionInfo


def get_dirs(version_info):
    return {
        d.replace(typeshed.TYPESHED_PATH, "").lstrip(os.path.sep)
        for d in typeshed._get_typeshed_directories(version_info)
    }


def get_stub_files():
    def get_map(version_info):
        return typeshed._create_stub_map(version_info)

    map_ = typeshed._create_stub_map(TYPESHED_PYTHON3)
    return os.path.join(TYPESHED_PYTHON3, "functools.pyi")


TYPESHED_PYTHON3 = os.path.join(typeshed.TYPESHED_PATH, "stdlib", "3")
print("@", TYPESHED_PYTHON3)

print("----------------------------------------")

for d in get_dirs(PythonVersionInfo("3", "8")):
    print(d)

print("----------------------------------------")
print(get_stub_files().replace(os.path.expanduser("~"), "~"))


print("----------------------------------------")
print(jedi.Script("import string; string.capwords").usages())
