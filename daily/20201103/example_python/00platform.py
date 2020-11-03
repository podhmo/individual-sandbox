import sys
import pathlib

print(sys.platform)
# win32

p = pathlib.Path("/tmp/src/xxx.txt")
print(p.parent.with_name(p.parent.name + "_win") / p.name)
# \tmp\src_win\xxx.txt
