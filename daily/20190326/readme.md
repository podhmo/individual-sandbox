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

