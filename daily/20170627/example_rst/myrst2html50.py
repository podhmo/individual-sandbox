from docutils.core import publish_cmdline, default_description
from docutils.parsers.rst import directives
from sphinx.directives.code import LiteralInclude

directives.register_directive("literalinclude", LiteralInclude)
description = (
    u'Generates HTML 5 documents from standalone '
    u'reStructuredText sources ' + default_description
)

publish_cmdline(writer_name='html5', description=description)
