from prestring.python.parse import parse_string, dump_tree
from lib2to3.fixer_util import Name, Assign

t = parse_string("""
1 + 2 + 3 + 4  # => xxx
""".lstrip())

expr = t.children[0].children[0]
print(repr(expr))
xx = expr.clone()
xx.parent = None
expr.replace(Assign(Name("foo"), xx))
print(t)
dump_tree(t)
