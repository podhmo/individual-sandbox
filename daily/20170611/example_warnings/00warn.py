import warnings


def foo():
    warnings.warn("hmm")
    return "hai"


if __name__ == "__main__":
    print(foo())
