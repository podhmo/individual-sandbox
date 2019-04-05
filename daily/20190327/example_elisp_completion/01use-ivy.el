(require 'ivy)

(ivy-read "> " '("foo" "bar") :action #'insert)

(let ((ivy-display-function #'ivy-posframe-display-at-point))
  (ivy-read "> " '("foo" "bar") :action #'insert)
  )


(defun my:annotation-function (s)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item
      (setq my:item item)
      (setq my:s s)
      (setq my:minibuffer-completion-table minibuffer-completion-table)
      (format "-- %s" (second item)))
    ))

(let ((ivy-format-function #'ivy-format-function-default)
      (ivy-display-function #'ivy-posframe-display-at-point))
  (ivy-read "> " '("foo" "bar") :action #'insert)
  )

(defun my:eldoc-function ()
  ;; prpertize
  (upcase (thing-at-point 'word)))

(setq-local eldoc-documentation-function #'my:eldoc-function)
(let ((ivy-format-function #'ivy-format-function-arrow)
      (ivy-display-function #'ivy-posframe-display-at-point)
      )
  (let ((disabled (not (member 'eldoc-mode minor-mode-list))))
    (when disabled
      (eldoc-mode 1)
      )
    (ivy-read "> " '("foo" "bar") :action #'insert)
    (when disabled
      (eldoc-mode -1)
      )))

