import os
import sys


rfd, wfd = os.pipe()
pid = os.fork()
if pid == 0:
    # child process
    print("this is child")

    os.close(rfd)
    os.dup2(wfd, sys.stdout.fileno())
    os.close(wfd)
    print("text by child process")
    sys.exit(0)
else:
    print("this is parent")

    os.close(wfd)
    os.dup2(rfd, sys.stdin.fileno())
    os.close(rfd)

    print(sys.stdin.read(), pid)
    sys.exit(0)
