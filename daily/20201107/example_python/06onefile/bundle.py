import pathlib
from importlib.util import find_spec
from importlib.resources import contents
import queue

# import starlette

module_name = "starlette"
spec = find_spec(module_name)
print(spec)

code_map = {"@packages@": []}
q = queue.Queue()

for dirpath_str in spec.submodule_search_locations:
    q.put_nowait((pathlib.Path(dirpath_str), spec.name))

while not q.empty():
    dirpath, prefix = q.get_nowait()
    code_map["@packages@"].append(prefix.replace(module_name, "foo"))
    for filename in contents(prefix):
        p = dirpath / filename
        if p.is_dir():
            if "__pycache__" == filename:
                continue
            q.put_nowait((p, prefix + "." + filename))
            continue

        if not filename.endswith(".py"):
            continue

        with p.open() as rf:
            fullname = prefix + "." + filename[:-3]
            if fullname.endswith(".__init__"):
                fullname = fullname[: -len(".__init__")]

            replaced = fullname.replace(module_name, "foo")
            code_map[replaced] = rf.read()

with open(pathlib.Path(__file__).parent / "__embed__.py", "w") as wf:
    print(repr(code_map), file=wf)
print("@", code_map["@packages@"])
