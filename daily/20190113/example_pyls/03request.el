(require 'jsonrpc)

(setq my:process
      (make-process
       :name "my:pyls"
       :command (list (expand-file-name "~/venvs/my/bin/pyls"))
       :connection-type 'pipe
       :coding 'utf-8-emacs-unix
       :noquery t
       :stderr (get-buffer-create "*my:pyls stderr*")
       ))

(setq my:connection
      (jsonrpc-process-connection
       :name "pyls"
       :process my:process
       :on-shutdown #'(lambda (server) (print (list "bye" server)))
       ))


; (jsonrpc--request-continuations my:connection);; => #s(hash-table size 65 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())
(jsonrpc-request
 my:connection
 :initialize
 (list :processId (emacs-pid)
       :rootPath (expand-file-name default-directory)
       :rootUri (format "file://%s" default-directory)
       )
 :timeout 10)

(jsonrpc-request
 my:connection
 :textDocument/completion
 (list
  :textDocument (list :uri (format "file://%s/something.py" default-directory))
  :position (list :line 1 :character 7)
  ))

(jsonrpc-request
 my:connection
 :shutdown nil
)

(jsonrpc-request
 my:connection
 :exit nil
)

;; (display-buffer "*my:pyls stderr*")
;; (display-buffer "*my:pyls output*")
