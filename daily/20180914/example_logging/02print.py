import logging


class Wrapper:
    def __init__(self, logger, *, level):
        self.logger = logger
        self._write = getattr(logger, level)

    def write(self, b):
        if b.strip():
            self._write(b)


if __name__ == "__main__":
    logger = logging.getLogger(__name__)
    logging.basicConfig(level=logging.INFO)
    import contextlib
    with contextlib.redirect_stdout(Wrapper(logger, level="info")):
        import x
        x.f()


