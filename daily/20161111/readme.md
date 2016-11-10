# ocaml opam

opamのversin upと使うocamlのversionあげる

```
$ opam switch list
$ opam update
$ opam switch list  # 増える
$ opam switch 4.04.0
```

## memo

こういうの出るので注意

```
[WARNING] A conflict was detected in your installation. This can be caused by updated
constraints in your installed packages:
  - variantslib is not available because your system doesn't comply with ocaml-version = "4.02.3".

You should run "opam upgrade --fixup" to resolve the situation.
```

## install error 4.04.0

以下のようなエラーが出る

```
### stdout ###
# Configuring OCaml version 4.04.0
# Configuring for host x86_64-apple-darwin15.6.0 ...
# Configuring for target x86_64-apple-darwin15.6.0 ...
# Using compiler gcc.
# Compiler family and version: .
# [ERROR!] Unable to compile the test program.
#  Make sure the C compiler gcc  is properly installed.
### stderr ###
# + echo '* : g = 1'
# couldn't understand kern.osversion `15.6.0'
# i686-apple-darwin11-llvm-gcc-4.2: error trying to exec 'cc1': execvp: No such file or directory
```

古かったっぽい

```
$ sudo port install gcc49
$ sudo port select gcc mp-gcc49
$ opam switch 4.04.0
```

# ocaml emacs tail callかどうか調べる方法

`M-x caml-types-show-call` が楽かも。

もしくは真面目に `ocamlopt -annot` とかでcompileしてみる

- [example_annot](./example_annot)

