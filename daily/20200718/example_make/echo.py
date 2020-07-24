import shlex
import sys

print(shlex.join(sys.argv[1:]))
print("\t --", shlex.join(sys.argv), file=sys.stderr)
