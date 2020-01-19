import subprocess
import sys
import os
import queue
import threading
from handofcats import as_subcommand


@as_subcommand
def master() -> None:
    # breakpoint()
    pid = os.getpid()
    q = queue.Queue()

    cmd = [sys.executable, "-u", __file__, "slave", "--parent-id", str(pid)]
    p = subprocess.Popen(
        cmd, text=True, stdout=subprocess.PIPE, stdin=subprocess.PIPE, bufsize=-1
    )

    def _enqueue():
        for line in iter(p.stdout.readline, ""):
            print("@", line)
            q.put(line)

    th = threading.Thread(target=_enqueue, daemon=True)
    th.start()
    while True:
        print(q, q.qsize())
        while q.qsize():
            print("xxx", q.qsize())
            print("****", q.get())
        print(input("> "), file=p.stdin)
        p.stdin.flush()


@as_subcommand
def slave(*, parent_id: int) -> None:
    import time
    import sys

    print("\x1b[32mstart\x1b[0m", file=sys.stderr)
    i = 0
    for line in iter(sys.stdin.readline, ""):
        line = line.rstrip()
        print("C", i, line, file=sys.stdout)
        i += 1


as_subcommand.run()
