00 printing onesself

```console
$ python 00*.py
def Main():
    import argparse
    parser = argparse.ArgumentParser(help='fake')
    parser.add_argument('-c', '--config', required=True)
    parser.add_argument('--log-level', choices=('CRITICAL', 'FATAL', 'ERROR', 'WARN', 'WARNING', 'INFO', 'DEBUG', 'NOTSET'), default='INFO')
    args = parser.parse_args()
    main(**vars(args))


if __name__ == '__main__':
    Main()
```

01 the function as command

```console
$ python 01*.py -h
usage: 01ascommand.py [-h] -x X -y Y

optional arguments:
  -h, --help  show this help message and exit
  -x X
  -y Y

$ python 01*.py -x 10 -y 20
10 + 20 = 1020
```

