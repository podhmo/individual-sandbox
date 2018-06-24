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
    method_names = [name for name, kind in attrs if kind == "method"]
    content = doc.indent("".join([doc.document(getattr(this_cls, name)) for name in method_names]))
    mro = " <- ".join([cls.__name__ for cls in this_cls.mro()])
    return "\n".join([mro, content])


def grep_by_indent(s, level, rx=re.compile("^\s+")):
    for line in s.split("\n"):
        m = rx.search(line)
        if m is None or len(m.group(0)) <= level:
            yield line


from matplotlib.backends.backend_svg import RendererSVG  # noqa
# from collections import ChainMap
from wsgiref.simple_server import WSGIServer

for cls in WSGIServer.mro():
    if cls == object:
        break
    text = shape_text(cls)
    print("\n".join(grep_by_indent(text, 4)))
