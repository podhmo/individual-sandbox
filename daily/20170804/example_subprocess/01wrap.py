import os
import subprocess as s
import time


class P:
    def __init__(self, cmd):
        self.p = s.Popen(cmd, stdin=s.PIPE, stdout=s.PIPE, stderr=s.STDOUT, universal_newlines=True)

    def communicate(self, msg):
        self.p.stdin.write(msg)
        self.p.stdin.flush()
        return self.p.stdout.readline()

    def __del__(self):
        print("deleted")
        self.p = None  # for gc


def do():
    p = P(["python", "-i", "-q"])
    print(p.communicate("print('hai')\n"))


def f():
    for i in range(5):
        do()
        time.sleep(0.1)


print(os.getpid())
f()
print(s.run(["pstree", str(os.getpid())]))
