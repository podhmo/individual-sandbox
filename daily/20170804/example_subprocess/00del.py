import os
import subprocess as s
import time


def do():
    cmd = ["python", "-i", "-q"]
    p = s.Popen(cmd, stdin=s.PIPE, stdout=s.PIPE, stderr=s.STDOUT, universal_newlines=True)
    msg = "print('hai')\n"
    p.stdin.write(msg)
    p.stdin.flush()
    print(p.stdout.readline())


def f():
    for i in range(5):
        do()
        time.sleep(0.1)


print(os.getpid())
f()
print(s.run(["pstree", str(os.getpid())]))
