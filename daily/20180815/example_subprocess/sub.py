import sys
import os

print("sub, 111, stdout", file=sys.stdout)
print("sub, 222, stderr", file=sys.stderr)
print("sub, ENV A", os.environ.get("A"))
