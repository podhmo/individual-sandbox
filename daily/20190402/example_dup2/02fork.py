import os

print("start", os.getpid())
pid = os.fork()


if pid < 0:
    raise Exception("something wrong")
elif pid == 0:
    print("child process start", pid, "pid", os.getpid())
    import time

    time.sleep(1)
    print("child process stop", pid, "pid", os.getpid())
else:
    print("parent process", pid, "pid", os.getpid())
    print(os.wait())
