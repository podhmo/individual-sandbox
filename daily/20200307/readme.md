## 久しぶりにwindowsを開いてみる

- 半角・全角が遠い
- firefox accountを作ろうとして失敗した
- shellが遠い
- Ctrl + w で閉じないアプリケーションがある？
- 特定の位置でemacsを開くとかがやりづらい
- temrinal上でマウスなしにスクロールができない
- pbcopyのようなものがほしい -> clip
- tigのようなものが使いにくい?

### 半角・全角が遠い

autohotkeyを使うと良いのかもしれない。

```
$ scoop install autohotkey
```

### windows autohotkey

使い方がわかっていなかったのでここにscriptなどを書いていく。
基本的には `<filename>.hk` という形のファイルを作るっぽい。
そして実行は右クリックで実行みたいな感じ。

```
MsgBox, hello
```

defaultは関連付けられていないっぽい？install先はこのあたりなので頑張って探す。

```
Name: autohotkey
Description: The ultimate automation scripting language for Windows.
Version: 1.1.32.00
Website: https://www.autohotkey.com/
License: GPL-2.0-or-later (https://spdx.org/licenses/GPL-2.0-or-later.html)
Manifest:
  $HOME\scoop\buckets\extras\bucket\autohotkey.json
Installed:
  $HOME\scoop\apps\autohotkey\1.1.32.00
Binaries:
  autohotkey.exe compiler/ahk2exe.exe
```


### windows pbcopy

clipが使える

```
$ do something | clip
```

### windows shortcut

- Ctrl + e explorerを開く
- Ctrl + shift + s windowsの範囲を指定してスクリーンショットを取る
