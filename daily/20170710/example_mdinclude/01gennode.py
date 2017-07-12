from docutils.parsers.rst import Parser
from docutils.utils import new_document
from docutils.core import publish_from_doctree


def build_doc(name):
    doc = new_document(name)
    doc.settings.tab_width = 4
    doc.settings.character_level_inline_markup = "\ "
    doc.settings.file_insertion_enabled = True
    doc.settings.pep_references = "http://www.python.org/dev/peps/"
    doc.settings.rfc_references = "http://tools.ietf.org/html/"
    return doc


text = """
hello
========================================

this is content

subsection
----------------------------------------

- foo
- bar
- boo

.. include:: sub.rst
"""

p = Parser()
doc = build_doc("<nosource>")
p.parse(text, doc)
print(publish_from_doctree(doc, writer_name='pseudoxml').decode("utf-8"))
