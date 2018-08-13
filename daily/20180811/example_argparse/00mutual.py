def parse(argv=None):
    import argparse
    parser = argparse.ArgumentParser()
    limit_group = parser.add_mutually_exclusive_group()
    limit_group.add_argument("--limit", dest="limit", type=int)
    limit_group.add_argument("--no-limit", dest="limit", action="store_const", const=None)
    parser.set_defaults(limit=100)
    return parser.parse_args(argv)


print(parse(["--limit", "100"]))  # Namespace(limit=100)
print(parse(["--limit", "50"]))  # Namespace(limit=50)
print(parse(["--no-limit"]))  # Namespace(limit=None)
print(parse(["--no-limit", "--limit", "50"]))  # error: argument --limit: not allowed with argument --no-limit
