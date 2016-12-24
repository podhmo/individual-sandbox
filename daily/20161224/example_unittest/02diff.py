import difflib

first = """\
* one
* two
* bar
""".splitlines(keepends=True)

second = """\
* one
* twwo
* bar
""".splitlines(keepends=True)

print("".join(difflib.ndiff(first, second)))
print("**")
print("".join(difflib.unified_diff(first, second, fromfile="first", tofile="second")))
