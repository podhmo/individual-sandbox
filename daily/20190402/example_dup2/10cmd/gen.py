# python -u gen.py
import time
import sys
# import fcntl
# import sys
# import os


# print(sys.stdout.line_buffering)
# print(sys.stdout.write_through)
# sys.stdout.flush()
# sys.stdout.reconfigure(line_buffering=False, write_through=True)


# fcntl.fcntl(
#     sys.stdout.fileno(),
#     fcntl.F_SETFL,
#     fcntl.fcntl(sys.stdout.fileno(), fcntl.F_GETFL) | os.O_SYNC,
# )
for i in range(10):
    print(i)
    time.sleep(0.5)
