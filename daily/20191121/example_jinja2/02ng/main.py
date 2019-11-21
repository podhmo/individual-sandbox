import os.path
from jinja2 import Environment, FileSystemLoader

here = os.path.dirname(__file__)
e = Environment(loader=FileSystemLoader(here))
t = e.get_template("main.j2")
e.compile_templates(here, zip=None, log_function=print)
