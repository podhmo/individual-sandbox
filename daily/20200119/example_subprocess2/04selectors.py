import selectors
import subprocess
import sys
import handofcats


@handofcats.as_subcommand
def main():
    p = subprocess.Popen(
        [sys.executable, "-u", __file__, "sub"],
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


@handofcats.as_subcommand
def sub():
    import sys
    from time import sleep

    for i in range(10):
        print(f" x{i} ", file=sys.stderr)
        sleep(0.1)
        print(f" y{i} ")
        sleep(0.1)


handofcats.as_subcommand.run()
