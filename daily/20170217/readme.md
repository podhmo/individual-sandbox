# unix version check

```bash
function version_gte() {
  # $1 <= $2
  sort=$(which gsort || which sort)
  [ "$1" = "`echo -e "$1\n$2" | $sort -V | head -n1`" ]
}

function version_gt() {
  # $1 < $2
  version_gte && [ "$1" != "$2" ]
}

function is_unsupported_version() {
  [ ! $(version_gt "$1" "$2") ]
}

```

# unix grep 微妙に正規表現の書き方が違ってつらい

+にも()にもquoteが要る

```bash
$ docker ps | grep -po 'mongo:\(\d\+\.\)*\d\+' | cut -d ":" -f 2
```

```bash
$ echo 'mongo:3.4.2' | grep -po 'mongo:\(\d\+\.\)*\d\+' | cut -d ":" -f 2
3.4.2
```

そしてこれは `-P` のつもりで `-p` を使っていた。gnu grepにしないと `-P` はない。

```bash
$ sudo port install grep # gnu grep
$ echo 'mongo:3.4.2' | grep -P 'mongo:(\d+\.)*\d+' | cut -d ":" -f 2
3.4.2
```
