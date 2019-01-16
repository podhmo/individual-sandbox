(defun eglot--guess-contact (&optional interactive)
  "Helper for `eglot'.
Return (MANAGED-MODE PROJECT CLASS CONTACT).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((guessed-mode (if buffer-file-name major-mode)) ;; 'python-mode
         (managed-mode 'python-mode) ;; 'python-mode
         (project (or (project-current) `(transient . ,default-directory))) ;; "./x.py"
         (guess (cdr (assoc managed-mode eglot-server-programs
                            (lambda (m1 m2)
                              (or (eq m1 m2)
                                  (and (listp m1) (memq m2 m1))))))) ;; ("pyls")
         (guess (if (functionp guess)
                    (funcall guess interactive)
                  guess)) ;; ここでfuncallするのか
         (class (or (and (consp guess) (symbolp (car guess)) ;;  'eglot-lsp-server
                         (prog1 (car guess) (setq guess (cdr guess))))
                    'eglot-lsp-server)) ;; (<symbol> . <value>) の場合にはclass扱いそうではない場合には
         (program (and (listp guess) (stringp (car guess)) (car guess))) ;; "pyls"
         (base-prompt
          (and interactive
               "Enter program to execute (or <host>:<port>): "))
         (program-guess
          (and program
               (combine-and-quote-strings (cl-subst ":autoport:"
                                                    :autoport guess))))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[eglot] Sorry, couldn't guess for `%s'!\n%s"
                              managed-mode base-prompt))
                     ((and program (not (executable-find program)))
                      (concat (format "[eglot] I guess you want to run `%s'"
                                      program-guess)
                              (format ", but I can't find `%s' in PATH!" program)
                              "\n" base-prompt)))))
         (contact
          (or (and prompt
                   (let ((s (read-shell-command
                             prompt
                             program-guess
                             'eglot-command-history)))
                     (if (string-match "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                       (string-trim s))
                         (list (match-string 1 s)
                               (string-to-number (match-string 2 s)))
                       (cl-subst
                        :autoport ":autoport:" (split-string-and-unquote s)
                        :test #'equal))))
              guess
              (eglot--error "Couldn't guess for `%s'!" managed-mode))))
    (list managed-mode project class contact)))

;; (list managed-mode project class contact)
`(python-mode (vc . "./xxx.py") 'eglot-lsp-server ("pyls"))
