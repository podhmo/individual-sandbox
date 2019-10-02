import sys
import os.path
import importlib.resources as resources

for fname in resources.contents("schemalint.management.resources"):
    if not os.path.splitext(fname)[1].endswith((".yaml", ".yml")):
        print("skip", fname, type(fname), file=sys.stderr)
        continue

    with resources.open_text("schemalint.management.resources", fname) as rf:
        from dictknife import loading
        print(loading.load(rf))
        print(rf)
