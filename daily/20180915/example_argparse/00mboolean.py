def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--foo", action="store_true")
    parser.add_argument("--mfoo", action="store_true", default=None)
    return parser.parse_args(argv)


print(main([]))
print(main(["--foo", "--mfoo"]))

# Namespace(foo=False, mfoo=None)
# Namespace(foo=True, mfoo=True)
