from docutils.parsers.rst import directives


if __name__ == "__main__":
    from writers._text import Writer
    from docutils.core import publish_cmdline, default_description
    from directives.hello import setup
    import logging
    setup(directives)
    logging.basicConfig(level=logging.DEBUG)
    description = "ReST to text ({})".format(default_description)
    publish_cmdline(writer=Writer(), description=description)
