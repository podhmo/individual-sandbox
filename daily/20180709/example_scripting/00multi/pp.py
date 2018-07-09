import json
from jinja2 import Template


t = Template("""\
{{name}}(age{{age}})
""")


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=argparse.FileType("r"))
    parser.add_argument("--format", default="json")
    args = parser.parse_args()
    with args.file as rf:
        d = json.load(rf)
        print(t.render(**d))


if __name__ == "__main__":
    main()
