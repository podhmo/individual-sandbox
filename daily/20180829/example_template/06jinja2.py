import os.path
import jinja2
from jinja2.utils import missing
from pprint import pprint
from tracelib import (  # ./tracelib.py
    Tracer,
    Env,
)
from handofcats import as_command
env = Env(tracer=Tracer([]))


class MakeUndefined(jinja2.Undefined):
    def __new__(self, hint=None, obj=missing, name=None):
        return env[name]


@as_command
def run(filepath: str) -> None:
    here = os.path.dirname(os.path.abspath(__file__))
    j2env = jinja2.Environment(loader=jinja2.FileSystemLoader(here), undefined=MakeUndefined)
    j2env.filters = env["filters"]
    template = j2env.get_template(filepath)
    template.render()
    pprint(vars(env))
