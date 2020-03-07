# dart

- https://dart.dev/guides
- https://dart.dev/guides/language/language-tour
- https://dart.dev/guides/language/effective-dart

## コマンド

```console
$ dart 00hello.dart
```

## dartで知りたいことってなんだろう？

- 言語仕様

  - class (constructor, property, classmethod, method)
  - function
  - import (export, foo.dartありのimportディレクトリのimport, show, hide)
  - control flow (while, for)
  - optional chaining
  - string interporation
  - (Extension method)

- 使い方

  - packageの使い方
  - packageの定義の仕方
  - testの書き方実行の仕方

- ライブラリ

  - unit test
  - option parser

- additionals

  - effective darts https://dart.dev/guides/language/effective-dart
  - extension methods https://dart.dev/guides/language/extension-methods
  - spec https://dart.dev/guides/language/spec
  - type systems https://dart.dev/guides/language/sound-dart

## project

- https://dart.dev/guides/packages
- https://dart.dev/guides/libraries/create-library-packages

projectの作成

```console
$ touch pubspec.yaml
```

pubspec.yaml

```yaml
name: m
dependencies:
  test:
```

このあと以下が必要。

```console
$ pub get
```

以下の様なファイルが生成される。

- pubspec.lock
- .packages
- .dart_tool

### upgrade

```console
$ pub upgrade
```

### package構成

package構成の話も載っていた。これは参考になる。

- https://dart.dev/guides/libraries/create-library-packages#organizing-a-library-package
- https://github.com/dart-lang/shelf

- example

  - example_server.dart

- lib

  - src

    - cascade.dart

  - shelf.dart
  - shelf_io.dart

- test

  - xxx_test.dart
  - yyy_test.dart

- pubspec.yaml

## testing

- https://dart.dev/guides/testing
- https://pub.dev/packages/test

```console
$ pub get
$ pub run test 04tests
```

## docstring

- https://dart.dev/guides/language/effective-dart/documentation#doc-comments
- https://github.com/dart-lang/dartdoc#dartdoc

どうやら `///` と "/" を3つつけるらしい。

```dart
/// The number of characters in this chunk when unsplit.
int get length => ...
```

pythonと同様にsummaryとdetailは一行改行する。

```dart
/// Deletes the file at [path].
///
/// Throws an [IOError] if the file could not be found. Throws a
/// [PermissionError] if the file is present but could not be deleted.
void delete(String path){
}
```

3人称単数で書くのか。
