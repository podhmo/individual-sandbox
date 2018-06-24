(require 'epc)
(eval-when-compile (require 'cl))

(message "Start EPC")

(defvar my-epc-server-py
  (expand-file-name "server.py"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defvar my-epc (epc:start-epc (or (getenv "PYTHON") "python")
                              (list my-epc-server-py)))

(message "Start request")

(deferred:$
  (epc:call-deferred my-epc 'add '(10))
  (deferred:nextc it
    (lambda (x) (message "Return : %S" x))))


(message "Return : %S" (epc:call-sync my-epc 'add '(10 40)))
