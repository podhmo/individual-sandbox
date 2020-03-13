## windows System Localの変更

- コントロールパネル > 時計と地域 > 管理タブ > システムロケールの変更

ワールドワイド言語サポートでUnicode UTF-8を使用をオンにする。

それとは別にemacsで変換・無変換でのIMEのon/offが効かない。

## windows shell

```console
python -c "import sys; print(sys.argv)" *
["-c", "*"]
```

powershellとcmd.exeではこのような結果になって期待したとおりではない。

### unicode

```console
$ chcp
現在のコード ページ: 932
$ chcp 65001
```

## fmtにstdinを渡す

### dartfmtn

errorを出す

```console
t> cat .\00hello.dart | dartfmt
Hit a bug in the formatter when formatting stdin.
Please report at: github.com/dart-lang/dart_style/issues
Invalid argument(s): Illegal character in path
#0      _Uri._checkWindowsPathReservedCharacters (dart:core/uri.dart:1732:11)
#1      _Uri._makeWindowsFileUrl (dart:core/uri.dart:1825:7)
#2      new _Uri.file (dart:core/uri.dart:1700:11)
#3      new StringSource (package:analyzer/src/string_source.dart:28:53)
#4      DartFormatter.formatSource (package:dart_style/src/dart_formatter.dart:103:24)
#5      formatStdin.<anonymous closure> (file:///C:/b/s/w/ir/cache/builder/sdk/third_party/pkg_tested/dart_style/bin/format.dart:231:30)
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

### gofmt

```console
$ cat main.go | gofmt
```

## vscode

- C-@ C-@ でterminalをtoggleできる

## scoop

xkeymacsとかをscoopに入れたい？

- https://repology.org/repository/scoop