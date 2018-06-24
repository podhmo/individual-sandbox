## emacs flycheck

checkerの内容を見たりする方法

```lisp
(flycheck-checker-get 'python-flake8 'start); => flycheck-start-command-checker
(flycheck-find-checker-executable 'python-flake8); => "$HOME/venv/my/bin/flake8"
(flycheck-checker-executable 'python-flake8); => "flake8"
(flycheck-checker-executable-variable 'python-flake8); => flycheck-python-flake8-executable
(flycheck-checker-get 'python-flake8 'command); => command
```

一部のcheckerを無効にする方法

```lisp
(defun my:python-flycheck-setup ()
  ;; this is buffer local
  (setq flycheck-disabled-checkers '(python-pylint python-pycompile))
  (flycheck-mode 1)
  )
(add-hook 'python-mode-hook 'my:python-flycheck-setup)
```

利用するコマンドを動的に変える方法

```lisp
(autoload 'flycheck-mode "flycheck" t)

(with-eval-after-load 'flycheck
  (defun my:flycheck-executable-find (command)
    (let ((command (if (stringp command) command (funcall command))))
      (executable-find command)
      ))

  (custom-set-variables
   '(flycheck-check-syntax-automatically '(save mode-enabled))
   '(flycheck-executable-find #'my:flycheck-executable-find) ; risky
   )
  )
```

flycheck-executable-findを書き換える

```lisp
(defun flycheck-find-checker-executable (checker)
  "Get the full path of the executbale of CHECKER.

Return the full absolute path to the executable of CHECKER, or
nil if the executable does not exist."
  (funcall flycheck-executable-find (flycheck-checker-executable checker)))

(defcustom flycheck-executable-find #'executable-find
  "Function to search for executables.

The value of this option is a function which is given the name or
path of an executable and shall return the full path to the
executable, or nil if the executable does not exit.

The default is the standard `executable-find' function which
searches `exec-path'.  You can customize this option to search
for checkers in other environments such as bundle or NixOS
sandboxes."
  :group 'flycheck
  :type '(choice (const :tag "Search executables in `exec-path'" executable-find)
                 (function :tag "Search executables with a custom function"))
  :package-version '(flycheck . "0.25")
  :risky t)
```

## emacs jedi

jediでbufferの位置によって実行するjediを変える。(コマンドが同じならpoolにcacheされるので大丈夫)

```
(defun my:python-jedi-setup ()
  (let ((cmds `(,(pickup:pickup-file "bin/python") ,@(cdr jedi:server-command)))
        (args '("--log-traceback")))
    (when cmds (set (make-local-variable 'jedi:server-command) cmds))
    (when args (set (make-local-variable 'jedi:server-args) args))
    )
  (jedi-mode 1)
  )
```


## emacs emacslisp

buffer local variable

```
(mapcar #'car (buffer-local-variables)) ;=>(buffer-display-time buffer-display-count buffer-invisibility-spec buffer-file-truename point-before-scroll buffer-auto-save-file-format buffer-file-format buffer-file-coding-system enable-multibyte-characters mark-active bidi-paragraph-direction local-abbrev-tabl...
```

微妙に意味が違うので注意

make-local-variables

> この関数は、カレントバッファにおいて symbol が(別の)値を持つようにします。 この変数のカレント値は、この変数を(まだ) バッファローカルにしていない (他 の)バッファでのデフォルト値として残されます。

make-variable-buffer-local

> この関数は、 symbol を(新規に作るバッファをも含め) 全てのバッファにおいて バッファローカルな変数にします。 make-variable-buffer-local を呼ぶと、 symbol のカレント値は(それ自身の)バッファローカル値を持たないバッファで見 られるデフォルト値になります。この変数の値を変えると、(前はデフォルトの値 であっても) カレントバッファにおける(別の)値を持つことになります。

### default値

setq-default

> この関数は、各 symbol のデフォルト値を、(それに対応する) value に設定しま す。(symbol は評価しませんが) value は評価します。最初の value を返します。デフォルト値は、 (固有の)バッファローカル値を持たないバッファで見られます。
> カレントバッファで symbol がバッファローカルでない場合、これは (この)カレ ントバッファで setq を行なうのと同じことになります。 symbol がカレントバッ ファでバッファローカルである場合、カレントバッファで見える値ではなく、他のバッファがまだバッファローカルな値を持たない場合にそこで見える値を設定しま す。 

```lisp
(make-variable-buffer-local 'local)
=> local

;; バッファ foo 中:
(setq local 'foo)
=> foo

;; バッファ bar 中:
local
=> nil
(setq-default local 'default)
=> default
local
=> default
(setq local 'bar)
=> bar

;; バッファ baz 中:
local
=> default

;; バッファ foo 中:
(setq local 'foo)
=> foo
(default-value 'local)
=> default
```

## emacs emacslisp

```lisp
(make-local-variable 'ys)

(defun my:ys ()
  (cond ((boundp 'ys) ys)
        (t (set 'ys (pickup:pickup-file "bin/flake8"))
           (print "read")
           ys)))
(my:ys)
```

## emacs emacslisp

変数を探してあったらそれを使う

```lisp
(defun my:get ()
  (let ((var 'my-var))
    (or
     (and (boundp var) (symbol-value var))
     'default
     )))

(my:get); => default

(setq my-var 'override)
(my:get); => override

(setq my-var nil)
(my:get); => default
```
