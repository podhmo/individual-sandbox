from kamidana import as_filter
from jinja2.filters import contextfilter


@as_filter
@contextfilter
def tick(ctx, s):
    import pdb; pdb.set_trace()
