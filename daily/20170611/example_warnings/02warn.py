import warnings


def foo():
    warnings.warn("hmm")
    warnings.warn("hmm. foo is deprecated on next version", DeprecationWarning)
    warnings.warn("hmm. foo is deprecated on next version", PendingDeprecationWarning)
    return "hai"


if __name__ == "__main__":
    print(foo())
    print(foo())
