from kamidana import as_global
from datetime import datetime
from kamidana import as_filter


@as_global
def now():
    return datetime.now()


@as_filter
def asdate(dt):
    return dt.strftime("%Y/%m/%d")
