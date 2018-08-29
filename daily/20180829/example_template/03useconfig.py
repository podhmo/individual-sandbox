from dictknife import loading


class Context:
    def __init__(self, args, defaults):
        self.args = args
        self.defaults = defaults

    def __getattr__(self, name):
        val = getattr(self.args, name, None)
        if val is not None:
            return val
        try:
            return self.defaults[name]
        except KeyError as e:
            raise AttributeError(str(e))


def run(args):
    config = loading.loadfile(args.config)
    template = config["template"].strip()
    c = Context(args, config.get("defaults") or {})
    print(template.format(c=c))


def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('-c', '--config', required=True)
    parser.add_argument('--name', required=False)
    args = parser.parse_args(argv)
    run(args)


if __name__ == '__main__':
    main()
