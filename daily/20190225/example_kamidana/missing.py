from kamidana import as_global


@as_global
def url_for(prefix, *, filename):
    return f"{prefix}/{filename}"
