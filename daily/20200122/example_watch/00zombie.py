import os
import time

pid = os.fork()
if pid == 0: # child process
    print("@exit", pid, os.getpid())
    exit(0)

while True:
    print("@@hmm", pid, os.getpid())
    time.sleep(1)
