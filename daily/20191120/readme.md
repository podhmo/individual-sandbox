## 何をしよう？

- UIが欲しい -> flutter ?
- RPCが欲しい -> hashicorp/go-plugin ?
- goaとの違いをはっきりとさせる
- 良い感じのviewerを？
- 何かのvisualizeを？
- react -> ひさしぶりにふくしゅうしてみる？

## arch yay -Syu

- emacs上でのIMの変換候補の表示が戻った
- alacrittyのフォントの見た目がひどくなった

### emacs

- そう言えば文字入力中の変換候補がでなくなっていたのが直ったな。
- flake8が効かなくなった。flycheckで。
- これは flycheck-verify-checker をすれば良い

### pipがない

https://docs.python.org/ja/3/library/ensurepip.html

```console
$ python -m pip
/usr/bin/python: No module named pip

# hmm
$ python -m ensurepip --user
$ python -m ensurepip --user --upgrade
$ python -m pip install flake8 --user
```

## alacritty

- fontが崩れるのを直したい

## mypy Annotatedはまだ未実装っぽい

pep593のやつ。

https://www.python.org/dev/peps/pep-0593/

typing_extensionsを使って利用はできるがmypyはエラーを出す。

## go-plugin

- https://github.com/hashicorp/go-plugin

```
$ go get -d -u github.com/hashicorp/go-plugin
```

この辺を見る

- https://github.com/hashicorp/go-plugin/tree/master/examples/basic


### hmm

- https://github.com/hashicorp/yamux

## htmlのこと

以下のことを久し振りにすると忘れる

- cssの読み込み
- jsのdeferなどの意味
- unpkg.comの使いかた
- 何らかのbuild tool

### dart

- [../20190810/readme.md](../20190810/readme.md)

## react

- parcelを使うのが楽そう

### 周辺

- styled-components
- emotion

### お手軽deploy

- now.sh
- next.js

### parcel

- https://parceljs.org/recipes.html

```console
$ npm install --save react react-dom
$ npm install --save-dev parcel-bundler
$ ./node_modules/.bin/parcel index.html
```

### tutorial

- https://ja.reactjs.org/tutorial/tutorial.html
- https://sbfl.net/blog/2019/02/20/react-only-tutorial/

日本語もあるのか。後者はunpkg.comを使うスタイル。

## deno

reactなどの環境が手軽に手に入るなら。。

### install

その前にdenoを入れないとか。

- https://github.com/denoland/deno_install

```console
$ curl -fsSL https://deno.land/x/install/install.sh | bash -x -u -s v0.2.10
```

どうやら `$HOME/.deno/bin/deno` にインストールされる

- https://deno.land/std/manual.md#an-implementation-of-the-unix-cat-program

hello world

### かんたんなツール

- https://dev.to/kryz/write-a-small-api-using-deno-1cl0

### servest

- https://servestjs.org/

