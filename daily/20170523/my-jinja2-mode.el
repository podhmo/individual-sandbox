(require 'derived)
(require 'sgml-mode)

(define-derived-mode my:jinja2-mode sgml-mode "my jinja2 mode")

(setq my:jinja2-mode-key-pair
      '(("<" . ">")
        ("{"  "}" "{")
        ("[" "]" "[")
        ("\"" . "\"")
        ("'" . "'")
        ("%" . "%")
        ))

(defun my:jinja2-mode-setup ()
  (require 'insert-pair-element nil t)
  (define-insert-pair-binding my:jinja2-mode-map my:jinja2-mode-key-pair)
  )

(add-to-list 'auto-mode-alist '("\\.j2$" . my:jinja2-mode))
(add-to-list 'auto-mode-alist '("\\.jinja2$" . my:jinja2-mode))
(add-hook 'my:jinja2-mode-hook 'my:jinja2-mode-setup)
