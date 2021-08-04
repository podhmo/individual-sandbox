import re
import jinja2

rx = re.compile("x")
o = jinja2.Template("{{x}}").render(x=rx)
print(o)