from codegen import hello, byebye
def create_parser():
    import argparse
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(title='subcommands', dest='subcommand')
    subparsers.required = True

    sub_parser = subparsers.add_parser('hello', help=None)
    sub_parser.add_argument('--name', required=True)
    sub_parser.set_defaults(subcommand=hello)

    sub_parser = subparsers.add_parser('byebye', help=None)
    sub_parser.add_argument('--name', required=True)
    sub_parser.set_defaults(subcommand=byebye)

    return parser


def main():
    parser = create_parser()
    args = parser.parse_args()
    params = vars(args)
    action = params.pop('subcommand')

    # TODO positionals

    action(**params)


if __name__ == '__main__':
    main()
