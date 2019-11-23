import os.path
from jinja2 import Environment, FileSystemLoader
from jinja2.meta import find_undeclared_variables, find_referenced_templates

here = os.path.dirname(__file__)
e = Environment(loader=FileSystemLoader(here))
source, filename, _ = e.loader.get_source(e, "main.j2")

print(find_undeclared_variables(e.parse(source)))
print(list(find_referenced_templates(e.parse(source))))
