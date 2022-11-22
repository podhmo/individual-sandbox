# 

## python file watch

- watchdogが使える事は知っている
- 標準ライブラリだけでどうやるか？
- pythonなしの場合はどうなるか？
  - unix tail -F
  - windows ? 
- https://docs.python.org/ja/3/library/selectors.html

### tailの実装

- https://github.com/coreutils/coreutils/blob/master/src/tail.c
- [How can I tail a log file in Python? - Stack Overflow](https://stackoverflow.com/questions/12523044/how-can-i-tail-a-log-file-in-python)

どうやら、ionotifyとselectが使われている。
threadingで受け取るのが手軽そう？subprocessと。
これをasyncioにする意味はある？

### python linux

pyionotifyを使っている時点でwatchdogと一緒では？

- [pythonでinotifyを使ってみる。 - Blanktar](https://blanktar.jp/blog/2013/03/python-inotify)


### windows

- https://github.com/thekid/inotify-win
- [FileSystemWatcher.Filter Property (System.IO)](https://learn.microsoft.com/en-us/dotnet/api/system.io.filesystemwatcher.filter?source=recommendations)

### watchdog

- https://github.com/gorakhargosh/watchdog/tree/master/src/watchdog/observers