;;; foo-flymake.el --- A foo Flymake backend  -*- lexical-binding: t; -*-

(defvar-local foo--flymake-proc nil)

(defun foo-flymake (report-fn &rest _args)
  (unless (executable-find "foo-lint")
    (error "Cannot find a suitable foo"))

  (when (process-live-p foo--flymake-proc)
    (kill-process foo--flymake-proc))

  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       foo--flymake-proc
       (make-process
        :name "foo-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer "*foo-flymake*")
        :command '("foo-lint")
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (with-current-buffer source (eq proc foo--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp
                              "^:\\([0-9]+\\):\\(.*\\)$"
                              nil t)
                       for msg = (match-string 2)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1)))
                       ;; for type = (if (string-match "^warning" msg) :warning :error)
                       for type = (nth (random 3) '(:note :warning :error))
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc))
              )))))
      (process-send-region foo--flymake-proc (point-min) (point-max))
      (process-send-eof foo--flymake-proc))))
