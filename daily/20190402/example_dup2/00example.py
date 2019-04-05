import os
import sys


with open("output.dup2.txt", "a") as w:
    os.dup(w.fileno(), sys.stdout.fileno())
    print("hello")
print("ok")
