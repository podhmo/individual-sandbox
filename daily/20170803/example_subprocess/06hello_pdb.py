import subprocess as s
import threading
import time


def reader(p):
    for line in iter(p.stdout.readline, b""):
        if line != "":
            print("@", line, end="")


cmd = ["python", "-c", "import pdb; pdb.set_trace()"]
p = s.Popen(cmd, stdin=s.PIPE, stdout=s.PIPE, stderr=s.STDOUT, universal_newlines=True)
th = threading.Thread(target=reader, args=(p, ), daemon=True)
th.start()
p.stdin.write("print('hello')\n")
p.stdin.flush()
time.sleep(1)
try:
    p.wait(1)
    print("end")
except s.TimeoutExpired as e:
    print("timeout", vars(e))
    p.stdin.close()
    print(p.wait(1))
