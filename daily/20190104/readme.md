## elisp ファイル監視にfilenotifyというパッケージが遭ったのか

```
;; This package is an abstraction layer from the different low-level
;; file notification packages `inotify', `kqueue', `gfilenotify' and
;; `w32notify'.
```


## elisp そういえば require clからrequire cl-libになった

## elisp elispでJSONの通信をどうしているのかということが知りたい感じ

使っていそうなのは

```
(require 'json)
(require 'jsonrpc)
(require 'url-parse)
(require 'url-util)
```

eglotのpylsを主体に見ていくか

### elisp eglotの中を覗いていく

どのあたりの関数がわかりやすいだろう？用途がわかりやすそうな名前は

- eglot-completion-at-point

ただしけっこうコードが長いので

- eglot-rename

の方がわかりやすいかも

```lisp
(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (jsonrpc-request (eglot--current-server-or-lose)
                    :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                           :newName ,newname))
   current-prefix-arg))
```

- `eglot--servfer-capable :renameProvider`
- `eglot--apply-workspace-edit`
- `jsonrpc-request`


基本的にはjsonrpcの形になっていれば `jsonrpc-request`で通信できちゃいそう。

わからないのは `eglot--apply-workspace-edit`

- weditってなんだろ？

  - edit,changes, documentChangesが含まれた値

- documentChangesからmapで実ファイルのパスに変換してる
- editsが変換対象
- `eglot--apply-text-edits` で文字列を適用している
- その後後片付け(なぜeldocがあるのかは謎)

```lisp
(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (eglot--uri-to-path uri) edits version)))
                   documentChanges))
          edit)
      (cl-loop for (uri edits) on changes by #'cddr
               do (push (list (eglot--uri-to-path uri) edits) prepared))
      (if (or confirm
              (cl-notevery #'find-buffer-visiting
                           (mapcar #'car prepared)))
          (unless (y-or-n-p
                   (format "[eglot] Server wants to edit:\n  %s\n Proceed? "
                           (mapconcat #'identity (mapcar #'car prepared) "\n  ")))
            (eglot--error "User cancelled server edit")))
      (while (setq edit (car prepared))
        (pcase-let ((`(,path ,edits ,version)  edit))
          (with-current-buffer (find-file-noselect path)
            (eglot--apply-text-edits edits version))
          (pop prepared))
        t)
      (unwind-protect
          (if prepared (eglot--warn "Caution: edits of files %s failed."
                                    (mapcar #'car prepared))
            (eglot-eldoc-function)
            (eglot--message "Edit successful!"))))))
```

重要なのはこの辺じゃないなjsonrpcのあたりだ。

```lisp
   (jsonrpc-request (eglot--current-server-or-lose)
                    :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                           :newName ,newname))
```

eglot--current-server-or-lose

```lisp
(defun eglot--current-server ()
  "Find the current logical EGLOT server."
  (or
   eglot--cached-current-server
   (let* ((probe (or (project-current)
                     `(transient . ,default-directory))))
     (cl-find major-mode (gethash probe eglot--servers-by-project)
              :key #'eglot--major-mode))))

(defun eglot--current-server-or-lose ()
  "Return current logical EGLOT server connection or error."
  (or (eglot--current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

```

project-currentという関数とても便利じゃない？

cacheはfile local variable?

```lisp
(defvar-local eglot--cached-current-server nil
  "A cached reference to the current EGLOT server.
Reset in `eglot--managed-mode-onoff'.")
```

## elisp そういえばsubr.elのマクロ使ったほうがわかりやすい気がする

- setq-local
- defvar-local

そういえばこれの元の関数どういう意味だったっけ？

[../20180624/readme.md]([../20180624/readme.md])

- setq-local current bufferだけにbuffer localを指定して値を格納
- defvar-local 新規につくる全てのbufferにおいてbuffer localにする

## elisp eglot lspのテストどうしているんだろう？

てきとうにeglotのテストコードを覗いてみる

```lisp
(ert-deftest basic-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point)
      (should (looking-back "sys.exit")))))
```

特にむずかしいことを考えずに`pyls`に依存しているのか(実際のところはpyls,rls,cargoあたりを調べている)。

project/something.pyに

```pyuthon
import sys
sys.exi
```

というコードを書いてあげて、コードを補完した結果 "sys.exit" を見つけられれば勝ちという感じ。

`eglot--with-fixture` がどうなっているのか調べないと。あと `eglot--tests-connect`。
shouldが一個だけの場合ってnilだとエラーになるのかな？(そうっぽい)

### 細々とした関数

`eglot--with-fixture`

なんか良い感じにfixtureを用意する便利関数っぽいな(macro)。

```lisp
(defmacro eglot--with-fixture (fixture &rest body)
  (declare (indent 1) (debug t))
  `(eglot--call-with-fixture
    ,fixture #'(lambda () ,@body)))

(defun eglot--call-with-fixture (fixture fn)
  "Helper for `eglot--with-fixture'.  Run FN under FIXTURE."
  (let* ((fixture-directory (make-temp-file "eglot--fixture" t))
         (default-directory fixture-directory)
         file-specs created-files
         syms-to-restore
         new-servers
         test-body-successful-p)
    (dolist (spec fixture)
      (cond ((symbolp spec)
             (push (cons spec (symbol-value spec)) syms-to-restore)
             (set spec nil))
            ((symbolp (car spec))
             (push (cons (car spec) (symbol-value (car spec))) syms-to-restore)
             (set (car spec) (cadr spec)))
            ((stringp (car spec)) (push spec file-specs))))
    (unwind-protect
        (let ((eglot-connect-hook
               (lambda (server) (push server new-servers))))
          (setq created-files (mapcan #'eglot--make-file-or-dir file-specs))
          (prog1 (funcall fn)
            (setq test-body-successful-p t)))
      (eglot--message
       "Test body was %s" (if test-body-successful-p "OK" "A FAILURE"))
      (unwind-protect
          (let ((eglot-autoreconnect nil))
            (mapc (lambda (server)
                    (condition-case oops
                        (eglot-shutdown
                         server nil 3 (not test-body-successful-p))
                      (error
                       (message "[eglot] Non-critical shutdown error after test: %S"
                                oops))))
                  (cl-remove-if-not #'jsonrpc-running-p new-servers)))
        (let ((buffers-to-delete
               (delete nil (mapcar #'find-buffer-visiting created-files))))
          (eglot--message "Killing %s, wiping %s, restoring %s"
                          buffers-to-delete
                          default-directory
                          (mapcar #'car syms-to-restore))
          (cl-loop for (sym . val) in syms-to-restore
                   do (set sym val))
          (dolist (buf buffers-to-delete) ;; have to save otherwise will get prompted
            (with-current-buffer buf (save-buffer) (kill-buffer)))
          (delete-directory fixture-directory 'recursive))))))
```

`eglot--tests-connect` 同期的に繋げるやつっぽい。

```lisp
(defun eglot--tests-connect (&optional timeout)
  (let* ((timeout (or timeout 2))
         (eglot-sync-connect t)
         (eglot-connect-timeout timeout))
    (apply #'eglot--connect (eglot--guess-contact))))
```

## elisp ertでテストを書く方法

- should
- should-not
- should-error
- should-not-err
- skip-unless


## elisp ertの使いかた

1. ert-deftestを書く
1. load-bufferをする
1. M-x ert

### batchで書く方法

```make
EMACS ?= emacs
SELECTOR ?= t

TESTS ?= $(wildcard *test.el)

test:
	$(EMACS) -Q --batch  -L . \
	    $(addprefix -l ,$(TESTS)) \
		--eval '(setq ert-batch-backtrace-right-margin 120)'	\
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'
```
