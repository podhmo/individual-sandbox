import os.path
from docutils.parsers import rst
from docutils.parsers.rst import directives
from docutils.core import publish_cmdline, default_description


class MDInclude(rst.Directive):
    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}

    def run(self):
        from recommonmark.parser import CommonMarkParser
        parser = CommonMarkParser()

        document = self.state.document
        filename = self.arguments[0]
        filepath = os.path.join(os.path.dirname(document.settings._source), filename)

        with open(filepath) as rf:
            text = rf.read()
            from docutils.utils import new_document
            subdocument = new_document(filepath)
            # passing subdocument (not document)
            parser.parse(text, subdocument)
        return subdocument.children


directives.register_directive("mdinclude", MDInclude)
description = (
    u'Generates HTML 5 documents from standalone '
    u'reStructuredText sources ' + default_description
)

publish_cmdline(writer_name='html5', description=description)
