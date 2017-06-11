import warnings
import logging


def foo():
    warnings.warn("hmm, this is deprecated", DeprecationWarning, stacklevel=2)
    warnings.warn("hmm")
    return "hai"


if __name__ == "__main__":
    # use: python -W default
    logging.basicConfig(level=logging.DEBUG)
    logging.captureWarnings(True)
    print(foo())
