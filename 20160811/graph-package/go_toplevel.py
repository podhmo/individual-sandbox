from functools import lru_cache
from collections import defaultdict
import subprocess
from subprocess import PIPE


@lru_cache(2 << 10)
def get_imports(package):
    cmd = "go list -f '{{{{ .Imports }}}}' {}".format(package)
    try:
        pr = subprocess.run(cmd, shell=True, check=True, stdout=PIPE)
        return pr.stdout.decode("utf-8")[1:-2].split(" ")
    except subprocess.CalledProcessError:
        sys.stderr.write("failed with 'go list {}'".format(package))
        return []


class Control:
    def __init__(self, skips):
        self.skips = skips
        self.nodes = defaultdict(list)  # node -> depends

    def has(self, s):
        return s in self.nodes

    def is_toplevel(self, s):
        return len(self.nodes.get(s)) <= 0

    def is_skip(self, s):
        return any(x in s for x in self.skips)

    def node_line(self, s):
        shape = 'shape="doublecircle"' if self.is_toplevel(s) else ''
        label = 'label="{}"'.format(s)
        options = ', '.join([label, shape])
        return '{normalized}[{options}];'.format(normalized=self.normalize(s), options=options)

    def edge_line(self, src, dst):
        return '{} -> {}'.format(self.normalize(src), self.normalize(dst))

    def normalize(self, s):
        return s.replace("/", "_").replace(".", "_")

    def assign(self, s):
        self.nodes[s] = []

    def subassign(self, parent, s):
        self.nodes[s].append(parent)


def graph_body(package, control, seen):
    if package in seen:
        return
    seen.add(package)

    if not control.has(package):
        control.assign(package)

    for dep in get_imports(package):
        if dep and not control.is_skip(dep):
            control.subassign(package, dep)
            yield control.edge_line(package, dep)
            yield from graph_body(dep, control, seen)


def main(*packages):
    control = Control(skips=["internal", "unsafe", "runtime"])
    # control = Control(skips=["internal", "unsafe", "runtime", "sync", "errors", "unicode"])
    [line for package in packages for line in graph_body(package, control, set())]
    nodes = [s for s in control.nodes.keys() if control.is_toplevel(s) and not control.is_skip(s)]
    import json
    return json.dumps(sorted(nodes), indent=2)

if __name__ == "__main__":
    import sys
    print(main(*sys.argv[1:]))
