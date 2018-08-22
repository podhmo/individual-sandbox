import difflib

x = "boo"
y = "bar"

for x, y in [[x, y], [x, x]]:
    print(x, y)
    print("----------------------------------------")
    print("".join(difflib.ndiff(x, y)))
    print("----------------------------------------")
    print("".join(difflib.context_diff(x, y)))
    print("----------------------------------------")
    print("".join(difflib.unified_diff(x, y)))
    print("========================================")
