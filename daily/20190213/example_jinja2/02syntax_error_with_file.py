import jinja2
from kamidana.debug import get_gentle_output_from_exception

filename = "hello.j2"
try:
    env = jinja2.Environment(loader=jinja2.FileSystemLoader(searchpath="."))
    t = env.get_or_select_template(filename)
    print(t.render(me="foo"))
except jinja2.TemplateSyntaxError as e:
    print(get_gentle_output_from_exception(e))
