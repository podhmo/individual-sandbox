## setup (arch)

```console
$ yay -S dart
```

## refs

- [language tour](https://dart.dev/guides/language/language-tour)
- [try dart](https://dartpad.dartlang.org/)

## hello world

```console
$ make 00
dart 00*.dart
Hello, World
```

## code formatter

```console
$ dartfmt <target file>

# overwrite
$ dartfmt -w <target file>
```

## use package

```console
$ cd <path-to-app>
$ pub get
```

pubspec.yaml (example)

```
name: myapp
dependencies:
  js: ^0.6.0
  intl: ^0.15.8
```

upgrade packages

```console
$ pub upgrade
```
