import subprocess as s
import threading
import time


def reader(p):
    for line in iter(p.stdout.readline, b""):
        if line != "":
            print("@", line, end="")


p = s.Popen(["irb"], stdin=s.PIPE, stdout=s.PIPE, stderr=s.STDOUT, universal_newlines=True)
th = threading.Thread(target=reader, args=(p, ), daemon=True)
th.start()
p.stdin.write("puts('*hello*')\n")
p.stdin.flush()
time.sleep(1)
try:
    p.wait(1)
    print("end")
except s.TimeoutExpired as e:
    print("timeout", vars(e))
    p.stdin.close()
    p.wait(1)
