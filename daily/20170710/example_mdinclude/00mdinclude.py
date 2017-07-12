import os.path
from docutils.parsers import rst
from docutils.parsers.rst import directives
from docutils.utils import new_document
from recommonmark.parser import CommonMarkParser
from docutils.core import publish_from_doctree, publish_doctree


class MDInclude(rst.Directive):
    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}

    def run(self):
        parser = CommonMarkParser()

        document = self.state.document
        filename = self.arguments[0]
        curdir = getattr(document.settings, "_source", None) or os.getcwd()
        filepath = os.path.join(curdir, filename)

        with open(filepath) as rf:
            text = rf.read()
            subdocument = new_document(filepath)
            parser.parse(text, subdocument)
        return subdocument.children


directives.register_directive("mdinclude", MDInclude)

text = """
hello(mdinclude)
========================================

this is content

subsection
----------------------------------------

- foo
- bar
- boo

.. mdinclude:: sub.md
"""

doc = publish_doctree(text)
print(publish_from_doctree(doc).decode("utf-8"))
