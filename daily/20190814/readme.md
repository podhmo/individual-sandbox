## 気になること

- panic recoveryのコードをもう少し短くできない？
- gqlgen+dataloader
- go-webtest続き
- toyquery続き
- yet another language service
- Emacsのlint,completion (flymake, eglot)
- flutterのwidget全体を眺める

## Emacsのlint

- 簡単なmajor-modeを作る
- このときのlintを用意してみる
- @を含んだ行はエラーとかの仕様でコマンドを作ってみる

### major-mode

#### 追記

- flycheckなどに渡せるのはmajor-modeだけだっけ?

なんか忘れてしまった。どういう方法があったのだっけ? [./about-emacs-major-mode.md](about-emacs-major-mode.md)

#### 追記

とりあえずテキトウにmajor-modeを作ってみた

- [./example-emacs-major-mode](./example-emacs-major-mode)
- faceなどを調べる方法を忘れがち (describe-text-properties)

#### 追記

flycheckでのエラーを作ってみる？[flycheck-pyflakes.el](https://github.com/Wilfred/flycheck-pyflakes/blob/master/flycheck-pyflakes.el)でも参考にすれば良い？

```lisp
(flycheck-define-checker python-pyflakes
    "lint"
  :command ("pyflakes" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes python-mode)
```

error-patterns を定義してあげれば良いのか

ちなみにpyflakesのエラーは以下の様な感じ

```console
$ pyflakes 00ng.py
00ng.py:1:5: invalid syntax
x = 
    ^
```

この辺見れば良さそう

- https://www.flycheck.org/en/latest/developer/developing.html
- https://igjit.github.io/posts/2019/04/flycheck-in-shell/

flycheck-set-checker-executable という存在を知ってしまった。

#### 謎のエラー

何か126で終了しているっぽい？

```
Suspicious state from syntax checker foo-lint: Flycheck checker foo-lint returned non-zero exit code 126, but its output contained no errors: emacs: VENV/individual-sandbox/daily/20190814/example-emacs-major-mode/foo-lint: Exec format error
```

実行権限がないらしい？あー。shebangタイポしてた `#!/bin/sh` ではなく `#/bin/sh` になってた。

以下の様な感じで調べてstart-processを実行していくと楽そう。

```
 (flycheck-start-current-syntax-check (flycheck-get-checker-for-buffer))

;; dir
 (flycheck-compute-working-directory (flycheck-get-checker-for-buffer))

;; cmd
 (flycheck-checker-get (flycheck-get-checker-for-buffer) 'start) ;=> flycheck-start-command-checker
 (flycheck-find-checker-executable (flycheck-get-checker-for-buffer))
 (flycheck-checker-substituted-arguments (flycheck-get-checker-for-buffer))
```

#### flycheckの設定の確認

- M-x flycheck-verify-setup
- M-x flycheck-verify-checker

#### こんなものが

- C-c ! C-w       flycheck-copy-errors-as-kill
- C-c ! s         flycheck-select-checker
- C-c ! l         flycheck-list-errors
- C-c ! n         flycheck-next-error
- C-c ! p         flycheck-previous-error

#### commandの定義を差し替える

実は色々機能があってコマンドの部分を差し替えられるのか。

```
     EXECUTABLE is a string with the executable of this syntax
     checker.  It can be overridden with the variable
     `flycheck-SYMBOL-executable'.  Note that this variable is
     NOT implicitly defined by this function.  Use
     `flycheck-def-executable-var' to define this variable.
```

#### 追記

新しくなったflymakeへの対応が無い気がする？

これを見るとflycheckよりflymakeの方が軽いらしい？

- https://qiita.com/Ladicle/items/feb5f9dce9adf89652cf

flymakeの設定作成方法についてあんまり書いているところがないな。

この辺を見ると良さそう。

- https://github.com/federicotdn/flymake-shellcheck/blob/master/flymake-shellcheck.el

flymake-diagnostic-fuctionsに追加してあげれば良さそう。

```lisp
(add-hook 'flymake-diagnostic-functions 'flymake-shellcheck--backend nil t)
```

あー、ここにやりかたが書いてあった。

https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html

というかこれ全体を読むべきですね。。

https://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top

古いものなのかなと思ったけれど。最初のスタート地点としては良いと書かれているな(http://elpa.gnu.org/packages/flymake.html)

> For programmers interested in writing a new Flymake backend, thedocstring of `flymake-diagnostic-functions', the Flymake manual, and the code of existing backends are probably a good starting point.

flymake-diagnostic-functions のヘルプを読んでいくと以下の様な感じ。

```lisp
(lambda (report-fn :key value :key2 value ...)
)
```

とりあえずflymakeの勉強からかも？statusは(wait, !, ?)
warning-minimum-log-level, warning-minimum-level

flymake-no-changes-timeout 長くて良い気がする？
flymake-start-on-newlineをdisableにするのもよいかも？

https://www.gnu.org/software/emacs/manual/html_node/flymake/Backend-functions.html#Backend-functions

この辺りを読むのが良いのかな。

- flymake-make-diagnostic で作った関数によって読まれる感じっぽい？

遅い場合にはasynchronous processでという話っぽい

- https://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html#Asynchronous-Processes

それでこのrubyの例にたどり着くのかー。

https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html#An-annotated-example-backend

flymake-proc-legacy-flymake?

#### 追記色々設定してみる(flymake)

```
Disabling backend flymake-proc-legacy-flymake because (error Can’t find a suitable init function)
```

hmm たぶん何かはいっている？

```lisp
flymake-diagnostic-functions
;; => (elisp-flymake-byte-compile elisp-flymake-checkdoc t)

(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake) 
```

hmm 動かない。

あー、ファイル名が表示されていなかった

### 追記 minor-mode

[../20160904/readme.md](../20160904/readme.md)

```lisp
(defvar  htodo-font-lock-keywords `((,(regexp-opt (list "todo" "ToDo" "Todo" "TODO")) . font-lock-warning-face)))

(defun htodo-turn-on ()
  (font-lock-add-keywords nil `(,@htodo-font-lock-keywords) t)
  )

(defun htodo-turn-off ()
  (font-lock-remove-keywords nil `(,@htodo-font-lock-keywords))
  )

(define-minor-mode htodo-mode
  "htodo-mode (minor version)" :lighter " htodo"
  (progn
    (if htodo-mode (htodo-turn-on) (htodo-turn-off))
    (font-lock-mode 1)
    )
  )
```

- https://ayatakesi.github.io/emacs/24.5/elisp_html/Defining-Minor-Modes.html
- https://ayatakesi.github.io/emacs/24.5/elisp_html/Keymaps-and-Minor-Modes.html#Keymaps-and-Minor-Modes

## emacs which-key

- https://github.com/justbur/emacs-which-key

## emacs flymake, flycheck

そういえば動作するタイミングってどんななんだっけ？

https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html

これによると

|  | Flycheck | Flymake |
| :--- | :--- | :--- |
| Checks after | save, newline, change | save, newline, change |

### flycheck

ここで調整できる

```lisp
(defcustom flycheck-check-syntax-automatically '(save
                                                 idle-change
                                                 new-line
                                                 mode-enabled)
"")
```

個人的には save, mode-enabledだけ

### flymake

固定で入っている

```
(define-minor-mode flymake-mode
;;...

    (add-hook 'after-change-functions 'flymake-after-change-function nil t)
    (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
    (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t)
```

flymake-after-change-functionsを止めたくなるかも。

```lisp
  (setq flymake-start-on-newline nil)
  (setq flymake-no-changes-timeout nil)
```


flymakeにもlistingが

- flymake-show-diagnostics-buffer
