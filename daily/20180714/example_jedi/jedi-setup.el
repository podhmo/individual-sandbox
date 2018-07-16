(defun my:python-jedi-setup ()
  (let ((alternative-python (pickup:pickup-file "bin/python")) ; pickup:pickup-file is magical function (custom defined)
        (cmds jedi:server-command)
        (args nil))

    (when alternative-python
      ;; venv/foo/bin/python -> venv/foo
      (let ((venv-path (file-name-directory (substring-no-properties (file-name-directory alternative-python) 0 -1))))
        (setq args (append args `("--virtual-env" ,venv-path))))
      (setq-local jedi:server-command cmds)
      (setq-local jedi:server-args args)
      )
    )

  (jedi-mode 1)

  (let ((map python-mode-map))
    (define-key map (kbd "M-.") 'jedi:goto-definition)
    (define-key map (kbd "M-,") 'jedi:goto-definition-pop-marker)
    (define-key map (kbd "C-c C-d") 'jedi:show-doc)
    )

  (add-to-list 'company-backends 'company-jedi)
  (company-mode-on))
;; (add-hook 'python-mode-hook 'my:python-jedi-setup)

