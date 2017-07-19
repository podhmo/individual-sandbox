from lib2to3 import pytree
from lib2to3 import pygram
from lib2to3.pgen2 import driver

default_driver = driver.Driver(pygram.python_grammar_no_print_statement, convert=pytree.convert)


def parse(code, parser_driver=default_driver):
    return parser_driver.parse_string(code, debug=True)


with open("hello.py") as rf:
    t =  parse(rf.read())
print(t)
t.children[0].children[1].value = "*replaced*"
t.children[0].children[4].children[1].prefix = "    # this is *replaced* comment\n"
print(t)
