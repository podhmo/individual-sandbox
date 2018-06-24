import pydoc
import re


def shape_text(this_cls, doc=pydoc.plaintext):
    attrs = [
        (name, kind) for name, kind, cls, _ in pydoc.classify_class_attrs(this_cls)
        if cls == this_cls
    ]
    attrs = [
        (name, kind) for name, kind in attrs if not (name.startswith("__") and name.endswith("__"))
    ]
    attrs = [(name, kind) for name, kind in attrs if not name.startswith("_")]
    method_names = [name for name, kind in attrs if kind == "method"]
    method_annotations = [
        "@OVERRIDE: " if any(c for c in this_cls.mro()[1:] if hasattr(c, name)) else ""
        for name in method_names
    ]
    method_docs = [
        prefix + doc.document(getattr(this_cls, name))
        for prefix, name in zip(method_annotations, method_names)
    ]

    content = doc.indent("".join(method_docs))
    mro = " <- ".join([cls.__name__ for cls in this_cls.mro()])
    return "\n".join([mro, content])


def grep_by_indent(s, level, rx=re.compile("^\s+")):
    for line in s.split("\n"):
        m = rx.search(line)
        if m is None or len(m.group(0)) <= level:
            yield line


# from collections import ChainMap as cls
# from wsgiref.simple_server import WSGIServer as cls
# from matplotlib.figure import Figure as cls
from matplotlib.backends.backend_mixed import MixedModeRenderer as cls
for cls in cls.mro():
    if cls == object:
        break
    text = shape_text(cls)
    print("\n".join(grep_by_indent(text, 4)))
