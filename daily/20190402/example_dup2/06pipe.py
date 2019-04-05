import os
import sys


rfd, wfd = os.pipe()
pid = os.fork()
if pid == 0:
    # child process
    os.close(rfd)
    with os.fdopen(wfd, "w") as wf:
        wf.write("text by child process")
    sys.exit(0)
else:
    os.close(wfd)
    with os.fdopen(rfd, "r") as rf:
        print(rf.read(), pid)
    sys.exit(0)
