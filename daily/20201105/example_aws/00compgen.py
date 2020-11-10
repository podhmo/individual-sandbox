import sys
import pathlib
import queue
import awscli.completer

dirpath = pathlib.Path("~/.config/compgen/default/aws").expanduser()
dirpath.mkdir(parents=True, exist_ok=True)

q = queue.Queue()
q.put(["aws"])

c = awscli.completer.Completer()
while not q.empty():
    path = q.get()
    if len(path) > 2:
        continue
    fpath = dirpath / ".".join(path)
    print(f"write {fpath}", file=sys.stderr)
    with open(fpath, "w") as wf:
        for line in c.complete(" ".join(path), point=None):
            print(line, file=wf)
            q.put([*path, line.strip()])
