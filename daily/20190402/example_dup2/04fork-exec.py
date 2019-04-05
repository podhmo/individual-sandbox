import os

pid = os.fork()
if pid == 0:
    # child process
    os.execvp("ls", ["ls", "-l"])
else:
    os.wait()
