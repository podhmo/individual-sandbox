# grep 微妙に正規表現の書き方が違ってつらい

+にも()にもquoteが要る

```bash
$ docker ps | grep -po 'mongo:\(\d\+\.\)*\d\+' | cut -d ":" -f 2
```

```bash
$ echo 'mongo:3.4.2' | grep -po 'mongo:\(\d\+\.\)*\d\+' | cut -d ":" -f 2
3.4.2
```
