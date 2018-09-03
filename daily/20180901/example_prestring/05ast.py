from prestring.python.parse import parse_file, dump_tree

t = parse_file("./hello.py")
print(str(t))
print("-")
dump_tree(t)
