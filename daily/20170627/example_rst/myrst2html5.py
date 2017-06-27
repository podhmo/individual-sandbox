import os.path
from docutils.core import publish_cmdline, default_description
from docutils.parsers import rst
from docutils import nodes
from docutils.parsers.rst import directives


class LiteralInclude(rst.Directive):
    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {'dedent': int, }

    def run(self):
        document = self.state.document
        dedent = self.options.get("dedent", 0)

        try:
            filename = self.arguments[0]
            filepath = os.path.join(os.path.dirname(document.settings._source), filename)
            with open(filepath) as rf:
                text = rf.read()

            if dedent > 0:
                text = "".join([line[dedent:] for line in text.splitlines(True)])
            retnode = nodes.literal_block(text, text, source=filename)
            self.add_name(retnode)
            return [retnode]
        except Exception as exc:
            return [document.reporter.warning(str(exc), line=self.lineno)]


directives.register_directive("literalinclude", LiteralInclude)
description = (
    u'Generates HTML 5 documents from standalone '
    u'reStructuredText sources ' + default_description
)

publish_cmdline(writer_name='html5', description=description)
