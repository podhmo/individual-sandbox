import inspect
import linecache
import fileinput

fname = inspect.getsourcefile(fileinput)
print(fname)
lines = linecache.getlines(fname)
for line in inspect.getblock(lines[82:]):
    print(line, end="")
