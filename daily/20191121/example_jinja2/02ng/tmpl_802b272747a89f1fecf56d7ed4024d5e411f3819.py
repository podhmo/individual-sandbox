from __future__ import division, generator_stop
from jinja2.runtime import LoopContext, TemplateReference, Macro, Markup, TemplateRuntimeError, missing, concat, escape, markup_join, unicode_join, to_string, identity, TemplateNotFound, Namespace, Undefined
name = 'main.j2'

def root(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    cond_expr_undefined = Undefined
    if 0: yield None
    parent_template = None
    pass
    parent_template = environment.get_template('./base.j2', 'main.j2')
    for name, parent_block in parent_template.blocks.items():
        context.blocks.setdefault(name, []).append(parent_block)
    yield from parent_template.root_render_func(context)

def block_head(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    cond_expr_undefined = Undefined
    if 0: yield None
    pass
    yield '\nHHHHHHHHHHHH'

blocks = {'head': block_head}
debug_info = '1=12'