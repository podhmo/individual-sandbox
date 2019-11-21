from __future__ import division, generator_stop
from jinja2.runtime import LoopContext, TemplateReference, Macro, Markup, TemplateRuntimeError, missing, concat, escape, markup_join, unicode_join, to_string, identity, TemplateNotFound, Namespace, Undefined
name = 'base.j2'

def root(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    cond_expr_undefined = Undefined
    if 0: yield None
    pass
    yield 'top'
    yield from context.blocks['head'][0](context)
    yield '\nbody'
    yield from context.blocks['tail'][0](context)

def block_head(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    cond_expr_undefined = Undefined
    if 0: yield None
    pass
    yield '\nhead'

def block_tail(context, missing=missing):
    resolve = context.resolve_or_missing
    undefined = environment.undefined
    cond_expr_undefined = Undefined
    if 0: yield None
    pass
    yield '\ntail'

blocks = {'head': block_head, 'tail': block_tail}
debug_info = '1=12&3=14&1=16&3=24'