import logging


class Wrapper:
    def __init__(self, logger, *, level):
        self.logger = logger
        self._write = getattr(logger, level)

    def write(self, b):
        if b.strip():
            self._write(b)


logger = logging.getLogger(__name__)
wrapper = Wrapper(logger, level="info")

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    from functools import partial
    import unittest.mock as mock
    with mock.patch("builtins.print", partial(print, file=wrapper)):
        import x
        x.f()
