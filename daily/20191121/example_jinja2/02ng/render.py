import os.path
from jinja2 import Environment, FileSystemLoader

here = os.path.dirname(__file__)
e = Environment(loader=FileSystemLoader(here))
t = e.get_template("main.j2")
print(t.render())
