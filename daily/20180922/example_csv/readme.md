#[python][csv]CSVに色を付けてみるととても見やすいという話
twitterで[CSVに色を付けてみるととても見やすいという話](https://mobile.twitter.com/takanakahiko/status/1043116090967044097)があった。実際どうなんだろうと思ったのでテキトウに色を付けて出力してみることにした。

たしかに見やすいかもしれない。

![色付きの出力](https://cdn-ak.f.st-hatena.com/images/fotolife/p/podhmo/20180922/20180922160857.png)

(CSVのサンプルは[この記事](https://qiita.com/motoki1990/items/0274d8bcf1a97fe4a869)から拝借)

code

```python
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
```

## 追記

console上で見たい場合には`less -R`と繋げると良い

```
python 00colorful.py <csv file> | less -R
```

標準入力も受け取れるようにしても良いかもしれない。

## 追記

上のようにpythonでやるよりもエディタの拡張になっていたほうが便利かも。
