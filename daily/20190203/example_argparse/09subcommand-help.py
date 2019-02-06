from argparse import ArgumentParser

p = ArgumentParser("cmd")
sps = p.add_subparsers()
sps.required = True
sp = sps.add_parser("foo")
sp.add_argument("--bar")

p.print_usage()
sp.print_usage()


class MyParser(ArgumentParser):
    def parse_args(self, args=None, namespace=None):
        v, argv = self.parse_known_args(args, namespace)
        if argv:
            if args is None:
                import sys
                args = sys.argv[1:]
            args.append("-h")
            from gettext import gettext as _
            msg = _('unrecognized arguments: %s')
            self.epilog = "\n".join([self.epilog or "", msg % ' '.join(argv)])
            return self.parse_args(args, namespace=namespace)
        return v


p = MyParser("cmd", conflict_handler="resolve")
sps = p.add_subparsers()
sps.required = True
sp = sps.add_parser("foo")
sp.add_argument("--bar")
p.parse_args(["foo", "--boo"])
print("ok")

