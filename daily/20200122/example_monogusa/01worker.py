import subprocess
import selectors
import sys
from handofcats import as_subcommand

"""
subcommandで実行できるようにしてみる。実行する処理自体は固定。
"""


@as_subcommand
def manager():
    p = subprocess.Popen(
        [sys.executable, "-u", __file__, "worker", "--uid", str(1)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=False,  # for read1
    )

    sel = selectors.DefaultSelector()
    sel.register(p.stdout, selectors.EVENT_READ)
    sel.register(p.stderr, selectors.EVENT_READ)

    while True:
        for key, events in sel.select():
            data = key.fileobj.read1().decode()
            if not data:
                exit()
            if key.fileobj is p.stdout:
                print("STDOUT", data, end="")
            else:
                print("STDERR", data, end="", file=sys.stderr)


@as_subcommand
def worker(*, uid: int):
    import sys
    from time import sleep

    for i in range(10):
        print(f"{uid} x{i} ", file=sys.stderr)
        sleep(0.1)
        print(f"{uid} y{i} ")
        sleep(0.1)


as_subcommand.run()
