;; (describe-variable 'completion-extra-properties)
(setq candidates
      '(
        ("foo" "* foo")
        ("bar" "* bar")
        ("boobarboo" "* boobarboo")
        ("faoo" "* faoo")
        ))

(completing-read "> " candidates)
(completing-read "> " (completion-table-dynamic (lambda (s) candidates)))

;; completion-extra-properties
(defun my:annotation-function (s)
  (let ((item (assoc s minibuffer-completion-table)))
    (when item
      (format "-- %s" (second item)))
    ))

(let ((completion-extra-properties '(:annotation-function my:annotation-function)))
  (completing-read "> " candidates)
  )

(defun my:completion ()
  (interactive)
  (let ((completion-extra-properties
         '(:annotation-function my:annotation-function)))
    (completing-read "> " candidates)
    ))

(completion-in-region (point) (+ 1 (point)) candidates);f

(let ((completion-extra-properties
       '(:annotation-function my:annotation-function)))
  (completion-in-region (point) (+ 1 (point)) candidates));f

(require 'ivy)
(let ((completion-in-region-function 'ivy-completion-in-region))
  (completion-in-region (point) (+ 1 (point)) candidates))f

(require 'ivy-posframe)
(let ((completion-in-region-function 'ivy-completion-in-region)
      (ivy-display-function 'ivy-posframe-display-at-frame-center))
  (completion-in-region (point) (+ 1 (point)) candidates))f

