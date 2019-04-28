import argparse
from dictknife.jsonknife import get_resolver


def run(src: str) -> None:
    resolver = get_resolver(src)

    def gen(*, path):
        def on_schema(d, *, path):
            for k, prop in d.get("properties", {}).items():
                if not hasattr(prop, "get"):
                    # print("hmm")
                    continue
                path.append(k)
                if "$ref" in prop:
                    k = prop["$ref"].rsplit("/", 1)[-1]
                    prop = resolver.access_by_json_pointer(prop["$ref"])

                # type: str?, object?
                yield from on_schema(prop, path=path)
                path.pop()
            yield ("/".join(path), d)

        yield from on_schema(resolver.doc, path=path)

    for name, node in gen(path=["Top"]):
        print(name, node.get("type"))
    print("*ok*")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("src")
    args = parser.parse_args()
    run(args.src)


if __name__ == "__main__":
    main()
