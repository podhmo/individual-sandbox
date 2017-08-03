import sys
import subprocess as s


class P:
    def __init__(self, return_err=s.STDOUT):
        # self.p = s.Popen(["python"], stdin=s.PIPE, stdout=s.PIPE, stderr=s.STDOUT)
        # self.p = s.Popen(["python"], stdin=s.PIPE, stdout=s.PIPE, stderr=sys.stderr)
        # self.p = s.Popen(["irb"], stdin=s.PIPE, stdout=s.PIPE, stderr=s.STDOUT)
        self.p = s.Popen(["python"], stdin=s.PIPE, stdout=sys.stdout, stderr=sys.stderr)

    def __call__(self, line, newline=True):
        self.p.stdin.write(line.encode("utf-8"))
        if newline and not line.endswith("\n"):
            self.p.stdin.write(b"\n")
        self.p.stdin.flush()


import threading


def peeker(proc):
    for line in iter(proc.p.stdout.readline, b""):
        print("@", line)


proc = P()
proc("print(1)\n\n")
th = threading.Thread(target=peeker, args=(proc, ), daemon=True)
th.start()
# proc("print('hello\\nbye')")
# proc("1/0")
proc("print('x')\n\n")
proc("print(1)\n\n")
proc("print('x')\n\n")
try:
    print(proc.p.poll())
    proc.p.wait(1)
except Exception as e:
    print(vars(e))
import time
time.sleep(1)
print("yay")
import atexit


def on_shutdown():
    print("@")


atexit.register(on_shutdown)
