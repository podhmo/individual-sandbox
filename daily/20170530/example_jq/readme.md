```console
$ make 00
bash -x 00.bash src/00values0.json src/00values1.json || true
+ mkdir -p dst
+ diff -u src/00values0.json src/00values1.json
+ diff -u /dev/fd/63 /dev/fd/62
++ cat src/00values0.json
++ cat src/00values1.json
++ jq -S .
++ jq -S .
+ diff -u /dev/fd/63 /dev/fd/62
++ cat src/00values0.json
++ cat src/00values1.json
++ jq -S 'sort_by(.age)'
++ jq -S 'sort_by(.age)'
$ make 01
bash -x 01.bash src/01values0.json src/01values1.json || true
+ mkdir -p dst
+ diff -u src/01values0.json src/01values1.json
+ diff -u /dev/fd/63 /dev/fd/62
++ cat src/01values0.json
++ cat src/01values1.json
++ jq -S --slurp .
++ jq -S --slurp .
+ diff -u /dev/fd/63 /dev/fd/62
++ cat src/01values0.json
++ cat src/01values1.json
++ jq -S --slurp 'sort_by(.age)'
++ jq -S --slurp 'sort_by(.age)'
```
