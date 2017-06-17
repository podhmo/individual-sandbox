# http://docutils.sphinx-users.jp/docutils/docs/howto/rst-directives.html
from docutils.parsers.rst import directives

if __name__ == "__main__":
    from writers._markdown import Writer
    from directives.hello import setup
    from docutils.core import publish_cmdline, default_description
    setup(directives)
    description = "ReST to markdown ({})".format(default_description)
    publish_cmdline(writer=Writer(), description=description)
