import re

s = """
- foo
- bar
- boo
""".strip()

print("answer 1")
print(re.sub('(?:^)|(\n)', '\\1\t', s))

print("answer 2")
print("\n".join("\t{}".format(line) for line in s.split("\n")))
