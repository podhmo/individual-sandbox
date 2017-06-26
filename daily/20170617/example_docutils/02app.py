from io import StringIO
from docutils.writers import Writer
from docutils import nodes


class Writer(Writer):
    def __init__(self):
        super().__init__()
        self.translator_class = MarkdownTranslator

    def translate(self):
        self.visitor = visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = visitor.io.getvalue().rstrip("\n")


class MarkdownTranslator(nodes.NodeVisitor):
    def __init__(self, document):
        super().__init__(document)
        self.io = StringIO()
        self.section_level = 1

    def visit_section(self, node):
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_title(self, node):
        if self.section_level > 1:
            self.io.write("\n")
        self.io.write("#" * self.section_level)
        self.io.write(" ")
        self.io.write(node.astext())
        self.io.write("\n")
        raise nodes.SkipNode

    def visit_paragraph(self, node):
        self.io.write('\n')

    def depart_paragraph(self, node):
        self.io.write('\n')

    def visit_Text(self, node):
        self.io.write(node.astext())

    def visit_list_item(self, node):
        self.io.write("- ")
        for c in node.children:
            self.io.write(c.astext())
        self.io.write('\n')
        raise nodes.SkipNode

    def unknown_visit(self, node):
        pass

    def unknown_departure(self, node):
        pass


if __name__ == "__main__":
    from docutils.core import publish_cmdline
    publish_cmdline(writer=Writer())
