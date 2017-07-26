from parselib import parse_from_file, dump_tree

t = parse_from_file("./hello.py")
dump_tree(t)
print("----------------------------------------")
t = parse_from_file("./klass.py")
dump_tree(t)
