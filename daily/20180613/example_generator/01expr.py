import ast
from lib2to3 import pygram
from lib2to3 import pytree
from lib2to3.pgen2 import driver

default_driver = driver.Driver(pygram.python_grammar_no_print_statement, convert=pytree.convert)
t = default_driver.parse_string("print((yield i))\n", debug=True)
# Node(file_input, [Node(simple_stmt, [Node(power, [Leaf(1, 'print'), Node(trailer, [Leaf(7, '('), Node(atom, [Leaf(7, '('), Node(yield_expr, [Leaf(1, 'yield'), Leaf(1, 'i')]), Leaf(8, ')')]), Leaf(8, ')')])]), Leaf(4, '\n')]), Leaf(0, '')])
print(repr(t))

t = default_driver.parse_string("print(('x'))\n", debug=True)
# Node(file_input, [Node(simple_stmt, [Node(power, [Leaf(1, 'print'), Node(trailer, [Leaf(7, '('), Node(atom, [Leaf(7, '('), Node(yield_expr, [Leaf(1, 'yield'), Leaf(1, 'i')]), Leaf(8, ')')]), Leaf(8, ')')])]), Leaf(4, '\n')]), Leaf(0, '')])
print(repr(t))

t = default_driver.parse_string("print('x')\n", debug=True)
# Node(file_input, [Node(simple_stmt, [Node(power, [Leaf(1, 'print'), Node(trailer, [Leaf(7, '('), Node(atom, [Leaf(7, '('), Node(yield_expr, [Leaf(1, 'yield'), Leaf(1, 'i')]), Leaf(8, ')')]), Leaf(8, ')')])]), Leaf(4, '\n')]), Leaf(0, '')])
print(repr(t))

# Module(body=[Expr(value=Call(func=Name(id='print', ctx=Load()), args=[Yield(value=Num(n=1))], keywords=[]))])
print(ast.dump(ast.parse("print((yield 1))")))
