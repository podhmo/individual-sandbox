import warnings


def foo():
    warnings.warn("hmm")
    warnings.warn("hmm. foo is deprecated", DeprecationWarning)
    warnings.warn("hmm. foo is deprecated on next version", PendingDeprecationWarning)
    return "hai"


if __name__ == "__main__":
    print("1.")
    print("----------------------------------------")
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        print(foo())

    print("2.")
    print("----------------------------------------")
    with warnings.catch_warnings():
        warnings.simplefilter("always")
        print(foo())
