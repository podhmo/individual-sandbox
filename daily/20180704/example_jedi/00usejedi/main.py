import jedi
from dictknife.loading import dumps

defs = jedi.Script("""import marshmallow""", 1, 7, ".").goto_definitions()
print(dumps({d.name: d.module_path for d in defs}, format="json"))
# (ffap-python:find-program ffap-python:python-program-name)
