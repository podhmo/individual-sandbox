from __future__ import division, generator_stop
from jinja2.runtime import LoopContext, TemplateReference, Macro, Markup, TemplateRuntimeError, missing, concat, escape, markup_join, unicode_join, to_string, identity, TemplateNotFound, Namespace, Undefined
name = 'main.py'

def root(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    cond_expr_undefined = Undefined
    if 0: yield None
    pass
    yield 'import os.path\nfrom jinja2 import Environment, FileSystemLoader\n\nhere = os.path.dirname(__file__)\ne = Environment(loader=FileSystemLoader(here))\nt = e.get_template("main.j2")\nprint(t.render())\ne.compile_templates(here, zip=None, log_function=print)'

blocks = {}
debug_info = ''