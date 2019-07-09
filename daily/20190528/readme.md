## flutter tutorial動かしてみる？

やること

- install

### install

- https://flutter.dev/docs/get-started/install/linux
- https://storage.googleapis.com/flutter_infra/releases/stable/linux/flutter_linux_v1.5.4-hotfix.2-stable.tar.xz

```console
$ mkdir -p work/flutter && cd work/flutter
$ tar xfv ~/Downloads/flutter*.xz
$ . ~/.bashrc # PATH追加
$ flutter doctor
```

.bashrc

```
# flutter
if [ -d $HOME/work/flutter]; then
  export PATH=~/work/flutter/flutter/bin:$PATH
fi
```

謎

```console
$ flutter precache
```

upgradeは (詳細は https://flutter.dev/docs/development/tools/sdk/upgrading)

```console
$ flutter upgrade
```

### doctor

どうもandroid studioか何かが必要？

```console
$ flutter doctor
Doctor summary (to see all details, run flutter doctor -v):
[✓] Flutter (Channel stable, v1.5.4-hotfix.2, on Linux, locale en_US.UTF-8)
[✗] Android toolchain - develop for Android devices
    ✗ Unable to locate Android SDK.
      Install Android Studio from: https://developer.android.com/studio/index.html
      On first launch it will assist you in installing the Android SDK components.
      (or visit https://flutter.dev/setup/#android-setup for detailed instructions).
      If the Android SDK has been installed to a custom location, set ANDROID_HOME to that
      location.
      You may also want to add it to your PATH environment variable.

[!] Android Studio (not installed)
[!] Connected device
    ! No devices available

! Doctor found issues in 3 categories.
```

ふつうに入れないとダメっぽい。[flutter-web](https://flutter.dev/docs/get-started/flutter-for/web-devs)を試していけばよかったかもしれない？

- https://developer.android.com/studio

```console
$ tar xvf ~/Downloads/andoroid-studio-ide*.tar.gz
$ cd android-studio/bin
$ ./studio.sh
$ flutter doctor --android-licenses

$ flutter doctor
Doctor summary (to see all details, run flutter doctor -v):
[✓] Flutter (Channel stable, v1.5.4-hotfix.2, on Linux, locale en_US.UTF-8)
[✓] Android toolchain - develop for Android devices (Android SDK version 28.0.3)
[!] Android Studio (version 3.4)
    ✗ Flutter plugin not installed; this adds Flutter specific functionality.
    ✗ Dart plugin not installed; this adds Dart specific functionality.
[!] Connected device
    ! No devices available

! Doctor found issues in 2 categories.

# 実行が./studio.shってマジ？
$ ./studio.sh
```

こういうのをインストールできれば良いらしい？

- https://plugins.jetbrains.com/plugin/9212-flutter
- https://plugins.jetbrains.com/plugin/6351-dart

`File > Settings (Preference) > Plugins`

```console
$ flutter doctor

Doctor summary (to see all details, run flutter doctor -v):
[✓] Flutter (Channel stable, v1.5.4-hotfix.2, on Linux, locale en_US.UTF-8)
[✓] Android toolchain - develop for Android devices (Android SDK version 28.0.3)
[✓] Android Studio (version 3.4)
[!] Connected device
    ! No devices available

! Doctor found issues in 1 category.
```

あと一歩。

- https://developer.android.com/studio/run/managing-avds?hl=ja
- https://developer.android.com/studio/run/index.html?hl=ja#RunningApp

`Run` した後 `Select Development Target` そのあとcreate virtual device。


- https://flutter.dev/docs/get-started/codelab

```console
$ flutter doctor
Doctor summary (to see all details, run flutter doctor -v):
[✓] Flutter (Channel stable, v1.5.4-hotfix.2, on Linux, locale en_US.UTF-8)

[✓] Android toolchain - develop for Android devices (Android SDK version 28.0.3)
[✓] Android Studio (version 3.4)
[✓] Connected device (1 available)

• No issues found!
```

avdmanager?

```
$ ~/Android/Sdk/tools/bin/avdmanager
```


### AVDの acceleration

- https://developer.android.com/studio/run/emulator-acceleration

### flutter web

- https://github.com/flutter/flutter_web

```console
$ git clone https://github.com/flutter/flutter_web.git
$ flutter pub global activate webdev
$ cd flutter_web/examples/hello_world
$ flutter pub upgrade
$ pub get
$ webdev serve
```

.bashrc

```
# flutter
if [ -d $HOME/work/flutter]; then
  export PATH=~/work/flutter/flutter/bin/cache/dart-sdk/bin:~/work/flutter/flutter/.pub-cache/bin:~/work/flutter/flutter/bin:$PATH
fi
```


### 暇つぶし

- https://medium.com/flutter-jp/flutter-learning-c5640c5f05b9
- http://ryuichi111std.hatenablog.com/entry/2018/09/09/232158
- https://qiita.com/Kata_Oka/items/944a6f00ec49eeec450b
