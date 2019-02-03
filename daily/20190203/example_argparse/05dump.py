import argparse
from dictknife import loading
from dictknife.commands._extra import apply_loading_format_extra_arguments_parser


def cat(*, format=None, extra=None):
    L = [
        {
            "name": "foo",
            "age": 20
        },
        {
            "name": "bar",
            "age": 21,
            "nickname": "B"
        },
        {
            "name": "boo"
        },
    ]
    loading.dumpfile(L, format=format, extra=extra)


def main():
    parser = argparse.ArgumentParser()

    subparsers = parser.add_subparsers(dest="subcommand")
    subparsers.required = True

    fn = cat
    sparser = subparsers.add_parser(fn.__name__, description=fn.__doc__)
    sparser.set_defaults(subcommand=fn)
    sparser.add_argument("-f", "--format", choices=loading.get_formats())
    apply_loading_format_extra_arguments_parser(sparser)

    args = parser.parse_args()
    params = dict(vars(args))

    params.pop("subcommand")(**params)


if __name__ == "__main__":
    main()
