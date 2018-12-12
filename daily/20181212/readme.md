## python selectors

primitiveな

- https://docs.python.org/ja/3/library/selectors.html

これディレクトリ上のファイルの生成とか監視できなくない？

https://github.com/emcrisostomo/fswatch

## arch go yay

yayのdownload時の表示作りたい


## python inotify asyncio

久しぶりに調べる

- https://github.com/rbarrois/aionotify
- https://github.com/wkhere/forever.py/blob/master/forever
- https://github.com/gorakhargosh/watchdog

たしかwatchdog動きが微妙だった気がする。aionotifyは小さいけれどlinux専用そう。

- mac,BSD kequeue
- linux inotify
- windows ?

もうちょっと違ったレイヤーで探すべき？

- https://github.com/aio-libs/aiohttp-devtools

## python houosekeeper

- ファイル監視にまじめに向き合う
- inotifyをthrottle
- ついでに通知ができて欲しい
- process実行中にqueueをsquashして次回の実行にしたい


## go notify

- https://github.com/fsnotify/fsnotify
