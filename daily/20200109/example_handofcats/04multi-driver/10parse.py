from prestring.python.parse import parse_file, PyTreeDumper

t = parse_file("cli-exposed.py")
PyTreeDumper().visit(t)
