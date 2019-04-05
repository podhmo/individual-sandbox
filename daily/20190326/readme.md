## emacs theme

- https://github.com/hlissner/emacs-doom-themes/tree/screenshots
- all-the-iconsで色々

## emacs counsel

elisp

- counsel-find-library
- counsel-describe-function
- counsel-describe-variables
- counsel-apropos

ui

- counsel-unicode-char
- counsel-describe-face
- counsel-faces

hmm bookmarkで良い感じ似できたら良い。検索 -> marking -> markingしていったものを巡回

### 絵文字

- counsel-unicode-char
- 表示は？ -> symbolaを入れるだけで大丈夫

```console
$ yay -S ttf-symbola
```

## go bingo

- $GOPATHが指定されていないときに`~/go`を見てくれない

## emacs elgot

lsp-serverを全部殺す方法。ただし自動で復帰してくる。

```lisp

(cl-loop for p in (process-list)
         do (let ((buf (process-buffer p)))
           (when (string-prefix-p "EGLOT" (process-name p))
             (kill-process p)
             (kill-buffer buf))
           )
         )
(process-buffer (car (process-list)))
```

### もう少し丁寧に読む

autoloadは？

それぞれどういう意味なんだろう？

- eglot -- LSPサーバーの動かして繋げる。
- eglot-ensure -- eglotのsessionを開始する

shutdownはM-x eglot-shutdown

```
(setq my:server (eglot--read-server ""))
(eglot-shutdown my:server)
(eglot--message "hai")

(eglot--capabilities my:server)
(setf (eglot--project my:server))
( (eglot--major-mode my:server))

(eglot--inhibit-autoreconnect my:server)
(eglot--managed-buffers my:server)
```

hmm

```
(cl-loop for server being the hash-values of eglot--servers-by-project
  when server
  do (print server))
```

### go-mode

defaultの設定を外していきたい


## python lsp lsp serverのお試し

- ["../20190313/example_mylsp"]("../20190313/example_mylsp")

やること

- [initialize](https://microsoft.github.io/language-server-protocol/specification#initialize)を実装
- shutdownを実装
- exitを実装

### 必須そう?

- go to definition
- textDocumentSync

### 便利そう？

- window/logMessage
- window/showMessage

### 参考になりそう？

- https://qiita.com/vain0x/items/d050fe7c8b342ed2004e

## emacs lsp eglot

```
Passing an initialize rootPath URI ("file:///tmp/") is deprecated. Use rootUri instead.
```

trace
```
client-request (id:1) Tue Mar 26 04:24:06 2019:
(:jsonrpc "2.0" :id 1 :method "initialize" :params
          (:processId 3689 :rootPath "/tmp/" :rootUri "file:///tmp/" :initializationOptions nil :capabilities
                      (:workspace
                       (:applyEdit t :executeCommand
                                   (:dynamicRegistration :json-false)
                                   :workspaceEdit
                                   (:documentChanges :json-false)
                                   :didChangeWatchedFiles
                                   (:dynamicRegistration t)
                                   :symbol
                                   (:dynamicRegistration :json-false))
                       :textDocument
                       (:synchronization
                        (:dynamicRegistration :json-false :willSave t :willSaveWaitUntil t :didSave t)
                        :completion
                        (:dynamicRegistration :json-false :completionItem
                                              (:snippetSupport :json-false)
                                              :contextSupport t)
                        :hover
                        (:dynamicRegistration :json-false)
                        :signatureHelp
                        (:dynamicRegistration :json-false :signatureInformation
                                              (:parameterInformation
                                               (:labelOffsetSupport t)))
                        :references
                        (:dynamicRegistration :json-false)
                        :definition
                        (:dynamicRegistration :json-false)
                        :documentSymbol
                        (:dynamicRegistration :json-false :symbolKind
                                              (:valueSet
                                               [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26]))
                        :documentHighlight
                        (:dynamicRegistration :json-false)
                        :codeAction
                        (:dynamicRegistration :json-false :codeActionLiteralSupport
                                              (:codeActionKind
                                               (:valueSet
                                                ["quickfix" "refactor" "refactor.extract" "refactor.inline" "refactor.rewrite" "source" "source.organizeImports"])))
                        :formatting
                        (:dynamicRegistration :json-false)
                        :rangeFormatting
                        (:dynamicRegistration :json-false)
                        :rename
                        (:dynamicRegistration :json-false)
                        :publishDiagnostics
                        (:relatedInformation :json-false))
                       :experimental nil)))
```

