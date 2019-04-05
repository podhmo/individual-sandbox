import os
import sys

with open("output.txt", "a") as f:
    os.dup2(f.fileno(), sys.stdout.fileno())
    os.execvp("wc", ["wc", sys.argv[0]])
