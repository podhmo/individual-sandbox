import jinja2

t = jinja2.Template(
    """
Hello.

this is {{me}}, what your name?
"""
)
print(t.render(me="foo"))
