from kamidana import as_global
from kamidana.compat import import_resources


@as_global
def get_grammar():
    return import_resources.read_text("lib2to3", "Grammar.txt")
