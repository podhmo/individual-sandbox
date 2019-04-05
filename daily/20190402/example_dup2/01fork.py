import os

print("start", os.getpid())
pid = os.fork()
if pid == 0:
    print("child process", pid, "pid", os.getpid())
else:
    print("parent process", pid, "pid", os.getpid())
