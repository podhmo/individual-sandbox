## python docutils docutils用のメモ

文字列からdoctreeを作る

```python
from docutils.core import publish_doctree
doc = publish_doctree(text)
```

doctreeから何かをwriteする

```python
from docutils.core import publish_from_doctree
publish_from_doctree(doc, writer_name="html5").decode("utf-8")
```

defaultのwriter_nameだと構造を把握しやすいかも(writer_name='pseudoxml')

parserを直接利用してみる

```python
from docutils.parsers.rst import Parser


def build_doc(name):
    doc = new_document(name)
    doc.settings.tab_width = 4
    doc.settings.character_level_inline_markup = "\ "
    doc.settings.file_insertion_enabled = True
    doc.settings.pep_references = "http://www.python.org/dev/peps/"
    doc.settings.rfc_references = "http://tools.ietf.org/html/"
    return doc


p = Parser()
doc = build_doc("<nosource>")
p.parse(text, doc)
```
