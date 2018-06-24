;; jedi
(with-eval-after-load 'python
  (require 'pickup) ; individual package

  (defun my:python-jedi-setup ()
    (let ((cmds `(,(pickup:pickup-file "bin/python") ,@(cdr jedi:server-command)))
          (args '("--log-traceback")))
      (when cmds (set (make-local-variable 'jedi:server-command) cmds))
      (when args (set (make-local-variable 'jedi:server-args) args))
      )
    (jedi-mode 1)

    (let ((map python-mode-map))
      (define-key map (kbd "M-.") 'jedi:goto-definition)
      (define-key map (kbd "M-,") 'jedi:goto-definition-pop-marker)
      (define-key map (kbd "C-c C-d") 'jedi:show-doc)
      )

    (add-to-list 'company-backends 'company-jedi)
    (company-mode-on)
    )

  ;; popwin
  (when (boundp 'popwin:special-display-config)
    (add-to-list 'popwin:special-display-config '("*jedi:doc*" :noselect))
    )

  (require 'jedi-core)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)

  ;; this is work-around
  (defun my:safe-python-jedi-setup ()
    (let ((p (start-process "find jedi" nil (pickup:pickup-file "bin/python")  "-c" "import jedi")))
      (set-process-sentinel
       p
       (lambda (p status)
         (cond ((= 0 (process-exit-status p)) (my:python-jedi-setup))
               (t (message "jedi is not found. please install `pip install jedi`"))))))
    )
  (add-hook 'python-mode-hook 'my:safe-python-jedi-setup)
  )
