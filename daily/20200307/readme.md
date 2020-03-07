## windows wsl

管理者権限で powershell を開く

```
Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
```

powershell で適当に実行
```
$ scoop install archwsl
$ arch
```

意外と時間がかかる

```
$ pacman-key --init;pacman-key --populate;pacman -Syy archlinux-keyring
$ pacman -S --needed dart make
```

```
$ scoop info wslarch
Name: archwsl
Description: Install ArchLinux as a WSL Instance
Version: 20.2.7.0
Website: https://github.com/yuk7/ArchWSL
Manifest:
  $HOME\scoop\buckets\extras\bucket\archwsl.json
Installed: No
Binaries:
  Arch.exe
Notes
-----
Even when you are logging in as 'root', some operations (like service command) require Windows administrator privileges
```

- https://github.com/yuk7/ArchWSL

```
$ scoop install archwsl
```

## dart dartをいじってみる？

### install

```
$ scoop install dart
```

flutterではdartはインストールされないんだろうか？（たぶんパスが通っていない場所に存在する）
まぁ別途インストールしちゃおう。

$HOME\.emacs.d


### 欲しくなった機能

最低限欲しくなった機能

- syntax highlight
- 対応するカッコを自動的に挿入する機能
- dartfmt
- (flycheck)


### dartfmt

パイプで渡すといけそうな雰囲気だけれど。エラーが出る。

```
$ cat xxx.dart | dartfmt
dartfmt : Hit a bug in the formatter when formatting stdin.
???? ?:1 ??:11
+ cat 03* | dartfmt 2>&1 |clip
+           ~~~~~~~~~~~~
    + CategoryInfo          : NotSpecified: (Hit a bug in th...rmatting stdin.:String) [], RemoteException
    + FullyQualifiedErrorId : NativeCommandError
 
Please report at: github.com/dart-lang/dart_style/issues
Invalid argument(s): Illegal character in path
#0      _Uri._checkWindowsPathReservedCharacters (dart:core/uri.dart:1732:11)
#1      _Uri._makeWindowsFileUrl (dart:core/uri.dart:1825:7)
#2      new _Uri.file (dart:core/uri.dart:1700:11)
#3      new StringSource (package:analyzer/src/string_source.dart:28:53)
#4      DartFormatter.formatSource (package:dart_style/src/dart_formatter.dart:103:24)
#5      formatStdin.<anonymous closure> (file:///C:/b/s/w/ir/cache/builder/sdk/third_party/pkg_tested/dart_style/bin/fo
rmat.dart:231:30)
#6      _RootZone.runGuarded (dart:async/zone.dart:1304:10)
#7      _BufferingStreamSubscription._sendDone.sendDone (dart:async/stream_impl.dart:391:13)
#8      _BufferingStreamSubscription._sendDone (dart:async/stream_impl.dart:401:15)
#9      _BufferingStreamSubscription._close (dart:async/stream_impl.dart:285:7)
#10     _SinkTransformerStreamSubscription._close (dart:async/stream_transformers.dart:98:11)
#11     _EventSinkWrapper.close (dart:async/stream_transformers.dart:25:11)
#12     _StringAdapterSink.close (dart:convert/string_conversion.dart:251:11)
#13     _Utf8ConversionSink.close (dart:convert/string_conversion.dart:302:20)
#14     _ConverterStreamEventSink.close (dart:convert/chunked_conversion.dart:82:18)
#15     _SinkTransformerStreamSubscription._handleDone (dart:async/stream_transformers.dart:143:24)
#16     _RootZone.runGuarded (dart:async/zone.dart:1304:10)
#17     _BufferingStreamSubscription._sendDone.sendDone (dart:async/stream_impl.dart:391:13)
#18     _BufferingStreamSubscription._sendDone (dart:async/stream_impl.dart:401:15)
#19     _BufferingStreamSubscription._close (dart:async/stream_impl.dart:285:7)
#20     _SyncStreamControllerDispatch._sendDone (dart:async/stream_controller.dart:774:19)
#21     _StreamController._closeUnchecked (dart:async/stream_controller.dart:631:7)
#22     _StreamController.close (dart:async/stream_controller.dart:624:5)
#23     _Socket._onData (dart:io-patch/socket_patch.dart:1838:21)
#24     _RootZone.runUnaryGuarded (dart:async/zone.dart:1316:10)
#25     _BufferingStreamSubscription._sendData (dart:async/stream_impl.dart:338:11)
#26     _BufferingStreamSubscription._add (dart:async/stream_impl.dart:265:7)
#27     _SyncStreamControllerDispatch._sendData (dart:async/stream_controller.dart:766:19)
#28     _StreamController._add (dart:async/stream_controller.dart:642:7)
#29     _StreamController.add (dart:async/stream_controller.dart:588:5)
#30     new _RawSocket.<anonymous closure> (dart:io-patch/socket_patch.dart:1386:35)
#31     _NativeSocket.issueReadEvent.issue (dart:io-patch/socket_patch.dart:892:18)
#32     _microtaskLoop (dart:async/schedule_microtask.dart:43:21)
#33     _startMicrotaskLoop (dart:async/schedule_microtask.dart:52:5)
#34     _runPendingImmediateCallback (dart:isolate-patch/isolate_patch.dart:118:13)
#35     _RawReceivePortImpl._handleMessage (dart:isolate-patch/isolate_patch.dart:175:5)
```

## 久しぶりにwindowsを開いてみる

- 半角・全角が遠い
- firefox accountを作ろうとして失敗した
- shellが遠い
- Ctrl + w で閉じないアプリケーションがある？
- 特定の位置でemacsを開くとかがやりづらい
- temrinal上でマウスなしにスクロールができない
- pbcopyのようなものがほしい -> clip
- tigのようなものが使いにくい?
- ディレクトリ内のファイル全体に対してgrepをかけたいかも -> ripgrep

### 半角・全角が遠い

autohotkeyを使うと良いのかもしれない。

```
$ scoop install autohotkey
```

### windows autohotkey

使い方がわかっていなかったのでここにscriptなどを書いていく。
基本的には `<filename>.ahk` という形のファイルを作るっぽい。
そして実行は右クリックで実行みたいな感じ。

```
MsgBox, hello
```

defaultは関連付けられていないっぽい？install先はこのあたりなので頑張って探す。
(追記: .ahkの間違いだった。かもしれない。これを書いていた当時は.hkがautohotkeyのファイル拡張子だと思っていた)

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

このあたりを参考にすれば良いっぽい

- http://yjo-blog.blogspot.com/2017/05/autohotkey-ime-onoff.html
- http://yjo-blog.blogspot.com/2016/02/altimeonoff.html
- http://blog.ayakix.com/2009/12/imeon-offautohotkey.html

keycodeを調べる

- http://ahkwiki.net/-InstallKeybdHook


### windows pbcopy

clipが使える

```
$ do something | clip
```

### windows shortcut

- Ctrl + e explorerを開く
- Ctrl + shift + s windowsの範囲を指定してスクリーンショットを取る

### ripgrep

```
$ scoop install ripgrep
$ rg path *.el | clip
*.el: ファイル名、ディレクトリ名、またはボリューム ラベルの構文が間違っています。 (os error 123)
$ ls *.el | % { rg path $_.FullName }
```

なかなか難しい。

- インストールして利用可能になるコマンドは "rg"
- clipでキャプチャできるのは標準出力だけの模様
- cwd内の全体に対して適用するときには、"ls *.el | % { rg path $_.FullName }" みたいな感じ

### pwd

```
$ pwd | clip
```

これがきかないのがつらい。

```
$ pwd | foreach { echo $_.Path } | clip
$ $PWD.Path | clip
```
