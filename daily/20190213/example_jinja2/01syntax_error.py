import jinja2
from kamidana.debug import get_gentle_output_from_exception

template = """
Hello.

this is {{me}, what your name?
this is {{me}, what your name?
"""
try:
    t = jinja2.Template(template)
    print(t.render(me="foo"))
except jinja2.TemplateSyntaxError as e:
    print(get_gentle_output_from_exception(e))
