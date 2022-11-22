import time
import subprocess
import select
import sys


# https://stackoverflow.com/questions/12523044/how-can-i-tail-a-log-file-in-python

filename = sys.argv[1]
f = subprocess.Popen(
    ["tail", "-F", filename], stdout=subprocess.PIPE, stderr=subprocess.PIPE
)
p = select.poll()
p.register(f.stdout)

while True:
    if p.poll(1):
        print(">>", f.stdout.readline())
    time.sleep(1)
