import sys
import subprocess
import queue
import json


def get_deps(pkg):
    print(f".. get deps {pkg}", file=sys.stderr)
    p = subprocess.run(
        ["go", "list", "-f", "{{.Imports}}", pkg],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    return p.stdout.strip("[']\n ").split(" ")


root = "cmd/go"
q = queue.Queue()
q.put(root)
d = {}

while not q.empty():
    pkg = q.get()
    if pkg in d:
        continue
    deps = [p for p in get_deps(pkg) if p.startswith("cmd/go/internal")]
    d[pkg] = deps
    for p in deps:
        q.put(p)

print(json.dumps(d, indent=2))
