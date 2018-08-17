import io
import subprocess
import sys


class IOWrapper(io.IOBase):
    def __init__(self, prefix, out=sys.stdout):
        self.prefix = prefix
        self.out = out

    def __getattr__(self, name):
        print("$", name)
        return getattr(self.out, name)

    def fileno(self):
        return 1

    def write(self, s):
        self.out.write(self.prefix)
        self.out.write(s)


ps = []
ps.append(subprocess.Popen(["python", "sub.py"], shell=False, stdout=subprocess.PIPE))
# ps.append(subprocess.Popen(["python", "sub.py"], shell=False, stdout=IOWrapper(":B:")))
for p in ps:
    print(p.stdout.read())
    p.wait()
