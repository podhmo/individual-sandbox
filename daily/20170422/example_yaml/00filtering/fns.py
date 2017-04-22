from zenmai.actions import load  # NOQA
from collections import ChainMap
from zenmai.decorators import with_context


def get_locals(scope, scopes=None):
    scopes = scopes or []
    scopes.append(scope.__dict__)
    if getattr(scope, "parent", None) is not None:
        return get_locals(scope.parent, scopes=scopes)
    else:
        return ChainMap(*scopes)


@with_context
def dynamic(transform, context):
    """teribble dangerous function"""
    local_values = get_locals(context.scope)
    r = eval(transform, {}, local_values)
    return r
