from parselib import parse_from_file, dump_tree

t = parse_from_file("./src/klass.py")
dump_tree(t)
