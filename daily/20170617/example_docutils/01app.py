import logging
from docutils.writers import Writer
from docutils import nodes
logger = logging.getLogger(__name__)


class Writer(Writer):
    def __init__(self):
        super().__init__()
        self.translator_class = Translator

    def translate(self):
        self.visitor = visitor = self.translator_class(self.document)
        self.document.walkabout(visitor)
        self.output = "*hmm*"


class Translator(nodes.NodeVisitor):
    def __init__(self, document):
        super().__init__(document)
        self._depth = 0

    def dispatch_visit(self, node):
        self._depth += 1
        return super().dispatch_visit(node)

    def dispatch_departure(self, node):
        r = super().dispatch_departure(node)
        self._depth -= 1
        return r

    def unknown_visit(self, node):
        i = self._depth
        nodename = node.__class__.__name__
        logger.debug("%svisit %s[%d] :%s", "  " * i, nodename, i, node)

    def unknown_departure(self, node):
        i = self._depth
        nodename = node.__class__.__name__
        logger.debug("%sdeparture %s[%d] :%s", "  " * i, nodename, i, node)


from docutils.core import publish_cmdline  # NOQA
logging.basicConfig(level=logging.DEBUG)
publish_cmdline(writer=Writer())
