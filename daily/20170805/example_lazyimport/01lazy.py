class reify(object):
    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except:
            pass

    def __get__(self, inst, objtype=None):
        if inst is None:
            return self
        val = self.wrapped(inst)
        setattr(inst, self.wrapped.__name__, val)
        return val


class Loading:
    @reify
    def json(self):
        import json
        return json

    @reify
    def yaml(self):
        import yaml
        from functools import partial

        class Dumper(yaml.Dumper):
            def represent_mapping(self, tag, mapping, flow_style=False):
                return super().represent_mapping(tag, mapping, flow_style=flow_style)

        yaml.dump = partial(yaml.dump, Dumper=Dumper)
        return yaml

    @reify
    def toml(self):
        import toml
        return toml


m = Loading()  # singleton


def run(loading):
    import sys
    data = {"person": {"name": "foo", "age": 20}}
    loading.dump(data, sys.stdout)
    sys.stdout.write("\n")
    sys.stdout.flush()


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--format", default="json", choices=["json", "yaml", "toml"])

    args = parser.parse_args()
    loading = getattr(m, args.format)

    run(loading)


if __name__ == "__main__":
    main()
