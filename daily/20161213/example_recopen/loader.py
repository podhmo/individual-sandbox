import os.path


class RecursivePathResolver(object):
    def __init__(self, path, history=None, registry=None):
        self.registry = registry or {}
        self.history = history or []
        self.path = path

    def __repr__(self):
        return "<Resolver path={!r} at {}>".format(self.path, hex(id(self)))

    def resolve_path(self, path):
        dirname = os.path.dirname(os.path.abspath(self.path))
        return os.path.normpath(os.path.join(dirname, path))

    def open(self):
        return open(self.path)

    def resolve(self, path):
        if path in self.registry:
            return self.registry[path]
        history = self.history[:]
        history.append(path)
        return self.__class__(self.resolve_path(path), history=history, registry=self.registry)


def _peek(resolver, data):
    import json
    indent = " " * len(resolver.history) * 2
    print(indent, "== ", resolver.path.replace(os.getcwd(), "."), " ==")
    for line in json.dumps(data, indent=2).split("\n"):
        print(indent, line)


def main(resolver):
    import json
    with resolver.open() as rf:
        data = json.load(rf)
        _peek(resolver, data)
        morning = resolver.resolve(data["morning"])

        with morning.open() as rf:
            data = json.load(rf)
            _peek(morning, data)

            leaf = morning.resolve(data["greeting"])
            with leaf.open() as rf:
                sdata = json.load(rf)
                _peek(leaf, sdata)
                print("morning: {}".format(sdata))

            end_greeting = morning.resolve(data["end-greeting"])
            with end_greeting.open() as rf:
                data = json.load(rf)
                _peek(end_greeting, data)

                leaf = end_greeting.resolve(data["greeting"])
                with leaf.open() as rf:
                    sdata = json.load(rf)
                    _peek(leaf, sdata)
                    print("evening: {}".format(sdata))

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--conf", required=False)
    args = parser.parse_args()

    resolver = RecursivePathResolver(args.conf or "./index.json")
    main(resolver)
