from jinja2.filters import contextfilter
from kamidana import as_filter


@as_filter
@contextfilter
def ng(ctx, s, default):
    if ctx.get("ok"):
        return s
    return default
