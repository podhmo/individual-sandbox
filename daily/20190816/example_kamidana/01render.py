import sys
from kamidana.driver import Driver
from kamidana.loader import TemplateLoader

import contextlib
from kamidana.debug.color import is_colorful, highlight
from kamidana.debug import gentleerror


@contextlib.contextmanager
def error_handler(*, quiet: bool, debug: bool):
    try:
        yield
    except Exception as e:
        if debug:
            raise
        if quiet:
            message = "{e.__class__.__name__}: {e}".format(e=e)
            print(highlight(message, colorful=is_colorful()), file=sys.stderr)
        else:
            print(
                gentleerror.translate_error(e, colorful=is_colorful()), file=sys.stderr
            )
        sys.exit(1)


debug = False
quiet = False
template = sys.argv[1]

with error_handler(debug=debug, quiet=False):
    loader = TemplateLoader([], [], [])

    driver = Driver(loader, format="raw")
    driver.run(template, None)
