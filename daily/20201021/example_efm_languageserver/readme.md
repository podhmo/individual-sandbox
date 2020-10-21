## emacs efm-languageserver

efm-languageserverをeglotから使ってみる

```
go get github.com/mattn/efm-langserver
mkdir -p ~/.config/efm-langserver
cp config.yaml ~/.config/efm-langserver/
```

markdownlintで試す。
(load-file "use.el")

## 上手く動かないなー

とりあえず、こうやるとdebugが豪華になるということがわかった

```console
$ efm-langserver -log-file /tmp/efm.log -loglevel 10
```

publishDiagnosticsが空なのがすごく気になる。

これを取りこぼしていた

```
2020/10/21 14:09:11 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
2020/10/21 14:09:11 markdownlint -s: env: node: No such file or directory

2020/10/21 14:09:11 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

あー。

```lisp
(executable-find "node") -> nil
(executable-find "markdownlint") -> t

;; こっちかと思ったら
(add-to-list 'exec-path (format "%s/.nvm/versions/node/v14.11.0/bin" (getenv "HOME")))
;; こっち
(setenv "PATH" (format "%s:%s" (format "%s/.nvm/versions/node/v14.11.0/bin" (getenv "HOME")) (getenv "PATH")))
```
