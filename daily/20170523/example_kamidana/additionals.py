import subprocess
from kamidana import as_filter


@as_filter
def upcase(v):
    return v.upper()


@as_filter
def oops(v):
    return "{}!!".format(v)


@as_filter
def spawn(cmd, encoding="utf-8"):
    p = subprocess.run(
        cmd,
        shell=True,
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    return p.stdout.decode(encoding)
