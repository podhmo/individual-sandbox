;; ( managed-mode project class contact)
`(python-mode (vc . "./xxx.py") 'eglot-lsp-server ("pyls"))


(defun eglot--connect (managed-major-mode project class contact)
  "Connect to MANAGED-MAJOR-MODE, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (car (project-roots project)))
         (nickname (file-name-base (directory-file-name default-directory)))
         (readable-name (format "EGLOT (%s/%s)" nickname managed-major-mode))
         autostart-inferior-process
         (contact (if (functionp contact) (funcall contact) contact))
         (initargs
                 `(:process ,(lambda ()
                               (make-process
                                :name readable-name
                                :command contact
                                :connection-type 'pipe
                                :coding 'utf-8-emacs-unix
                                :noquery t
                                :stderr (get-buffer-create
                                         (format "*%s stderr*" readable-name))))))
         (spread (lambda (fn) (lambda (server method params)
                                (apply fn server method (append params nil)))))
         (server
          (apply
           #'make-instance class
           :name readable-name
           :events-buffer-scrollback-size eglot-events-buffer-size
           :notification-dispatcher (funcall spread #'eglot-handle-notification)
           :request-dispatcher (funcall spread #'eglot-handle-request)
           :on-shutdown #'eglot--on-shutdown
           initargs))
         (cancelled nil)
         (tag (make-symbol "connected-catch-tag")))
    (setf (eglot--saved-initargs server) initargs)
    (setf (eglot--project server) project)
    (setf (eglot--project-nickname server) nickname)
    (setf (eglot--major-mode server) managed-major-mode)
    (setf (eglot--inferior-process server) autostart-inferior-process)
    ;; Now start the handshake.  To honour `eglot-sync-connect'
    ;; maybe-sync-maybe-async semantics we use `jsonrpc-async-request'
    ;; and mimic most of `jsonrpc-request'.
    (unwind-protect
        (condition-case _quit
            (let ((retval
                   (catch tag
                     (jsonrpc-async-request
                      server
                      :initialize
                      (list :processId (unless (eq (jsonrpc-process-type server)
                                                   'network)
                                         (emacs-pid))
                            :rootPath (expand-file-name default-directory)
                            :rootUri (eglot--path-to-uri default-directory)
                            :initializationOptions (eglot-initialization-options
                                                    server)
                            :capabilities (eglot-client-capabilities server))
                      :success-fn
                      (eglot--lambda ((InitializeResult) capabilities)
                        (unless cancelled
                          (push server
                                (gethash project eglot--servers-by-project))
                          (setf (eglot--capabilities server) capabilities)
                          (jsonrpc-notify server :initialized `(:__dummy__ t))
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              (eglot--maybe-activate-editing-mode server)))
                          (setf (eglot--inhibit-autoreconnect server)
                                (cond
                                 ((booleanp eglot-autoreconnect)
                                  (not eglot-autoreconnect))
                                 ((cl-plusp eglot-autoreconnect)
                                  (run-with-timer
                                   eglot-autoreconnect nil
                                   (lambda ()
                                     (setf (eglot--inhibit-autoreconnect server)
                                           (null eglot-autoreconnect)))))))
                          (run-hook-with-args 'eglot-connect-hook server)
                          (run-hook-with-args 'eglot-server-initialized-hook server)
                          (eglot--message
                           "Connected! Server `%s' now managing `%s' buffers \
in project `%s'."
                           (jsonrpc-name server) managed-major-mode
                           (eglot--project-nickname server))
                          (when tag (throw tag t))))
                      :timeout eglot-connect-timeout
                      :error-fn (eglot--lambda ((ResponseError) code message)
                                  (unless cancelled
                                    (jsonrpc-shutdown server)
                                    (let ((msg (format "%s: %s" code message)))
                                      (if tag (throw tag `(error . ,msg))
                                        (eglot--error msg)))))
                      :timeout-fn (lambda ()
                                    (unless cancelled
                                      (jsonrpc-shutdown server)
                                      (let ((msg (format "Timed out")))
                                        (if tag (throw tag `(error . ,msg))
                                          (eglot--error msg))))))
                     (cond ((numberp eglot-sync-connect)
                            (accept-process-output nil eglot-sync-connect))
                           (eglot-sync-connect
                            (while t (accept-process-output nil 30)))))))
              (pcase retval
                (`(error . ,msg) (eglot--error msg))
                (`nil (eglot--message "Waiting in background for server `%s'"
                                      (jsonrpc-name server))
                      nil)
                (_ server)))
          (quit (jsonrpc-shutdown server) (setq cancelled 'quit)))
      (setq tag nil))))
