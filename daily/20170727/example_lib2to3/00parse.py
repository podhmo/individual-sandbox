from parselib import parse_from_file

t = parse_from_file("./src/hello.py")
print(type(t))
print(t)
