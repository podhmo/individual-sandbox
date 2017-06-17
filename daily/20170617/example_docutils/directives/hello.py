# http://docutils.sphinx-users.jp/docutils/docs/howto/rst-directives.html
from docutils.parsers import rst
from docutils.parsers.rst import directives
from docutils import nodes


class HelloDirective(rst.Directive):
    """
    .. hello::
      :n: 10
    """
    option_spec = {"n": directives.positive_int}
    final_argument_whitespace = False
    has_content = True

    def run(self):
        n = self.options.get("n", 1)
        body = " ".join(["hello"] * n)
        return [nodes.Text("** {} **".format(body))]


def setup(directives=directives):
    directives.register_directive("hello", HelloDirective)
