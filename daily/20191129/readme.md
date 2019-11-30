## python argparse

add_subparsersのtitleは付けた方が良い

before

```
usage: run.py [-h] {foo,bar} ...

positional arguments:
  {foo,bar}

optional arguments:
  -h, --help  show this help message and exit
```

after

```
usage: run.py [-h] {foo,bar} ...

optional arguments:
  -h, --help  show this help message and exit

actions:
  {foo,bar}
```


## arch emojiの見た目を綺麗に

```
$ yay -Ss ttf-joypixels
```

### hmm

TODO

- https://wiki.archlinux.jp/index.php/%E3%83%95%E3%82%A9%E3%83%B3%E3%83%88
- https://github.com/joypixels/emojione/blob/master/extras/fonts/README.md
- https://github.com/laughk/archlinux-note/issues/21
