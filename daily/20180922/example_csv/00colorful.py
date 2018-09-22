import typing as t
import sys
import csv
import itertools

CSI = '\033['
RESET = f"{CSI}39m"


class ColorfulWriter(csv.DictWriter):
    def _dict_to_list(self, rowdict: t.Dict[str, str]) -> t.Sequence[str]:
        colors = itertools.cycle(range(31, 38))
        fields = super()._dict_to_list(rowdict)
        return [f"{CSI}{c}m{field}{RESET}" for c, field in zip(colors, fields)]


def run(f: t.IO, out: t.IO = sys.stdout) -> None:
    r = csv.DictReader(f)
    rows = list(r)
    w = ColorfulWriter(out, fieldnames=list(rows[0].keys()))
    w.writeheader()
    w.writerows(rows)
    out.write(RESET)


def main(argv=None):
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('f', type=argparse.FileType("r"))
    parser.add_argument('--out', required=False, default=sys.stdout)
    args = parser.parse_args(argv)
    run(**vars(args))


if __name__ == '__main__':
    main()
