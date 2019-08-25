from __future__ import division, generator_stop
from jinja2.runtime import LoopContext, TemplateReference, Macro, Markup, TemplateRuntimeError, missing, concat, escape, markup_join, unicode_join, to_string, identity, TemplateNotFound, Namespace
name = 'x.jinja2'

def root(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    if 0: yield None
    l_0_xxx = resolve('xxx')
    pass
    yield '\n    # access undefined\n    \n    %s # <- accessing undefined variable, here.\n    ' % (
        (undefined(name='xxx') if l_0_xxx is missing else l_0_xxx), 
    )

blocks = {}
debug_info = '4=12'