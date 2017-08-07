import os
import pyper
import time
import subprocess as s


def do():
    r = pyper.R()
    r("x <- 10")
    print(r["x"])


def f():
    for i in range(5):
        do()
        time.sleep(0.1)


print(os.getpid())
f()
print(s.run(["pstree", str(os.getpid())]))
