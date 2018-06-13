終了ステータスが0以外でコマンドが終了することをコマンドの失敗と言って良いのだっけ？

```console
# 止まらない
$ make run0
for i in 1 2 3; do python -c 'import sys; print(sys.argv); sys.exit(1 if int(sys.argv[1]) == 2 else 0)' $i; done
['-c', '1']
['-c', '2']
['-c', '3']

# 止まる
$ make run1
python -c 'import sys; print(sys.argv); sys.exit(0)' 1
['-c', '1']
python -c 'import sys; print(sys.argv); sys.exit(1)' 2
['-c', '2']
make: *** [two] Error 1
```
