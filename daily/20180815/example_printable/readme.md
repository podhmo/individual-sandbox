```console
$ python 02*.py -h
usage: 02ascommand-or-expose.py [-h] [--expose] -x X -y Y

optional arguments:
  -h, --help  show this help message and exit
  --expose
  -x X
  -y Y

$ python 02*.py -x 10 -y 20
10 + 20 = 1020

$ python 02*.py --expose


def run(*, x: int, y: int):
    print(f"{x} + {y} = {x + y}")



def main():
    import argparse
    parser = argparse.ArgumentParser(description=None)
    parser.print_usage = parser.print_help
    parser.add_argument('-x', required=True)
    parser.add_argument('-y', required=True)
    args = parser.parse_args()
    run(**vars(args))


if __name__ == '__main__':
    main()

$ python 03*.py -x 10 -y 20
10 + 20 = 1020
```

