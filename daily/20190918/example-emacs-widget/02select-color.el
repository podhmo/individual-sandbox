(defface asc:target-face
  '((t
     (:foreground "white" :background "#0080ff" :italic nil :bold t)))
  "face for selected by mouse")
(defvar asc:target-face 'asc:target-face)

(defun active-select-color () (interactive)
       (insert (propertize "target:\n" 'face asc:target-face))
       (widget-create 'editable-field
                      :size 6
                      :value "ffffff"
                      :notify
                      (lambda (w &rest ignore)
                        (let1 color (format "#%s" (string-trim (widget-value w)))
                          (message "edit *%s* (%s)" color (string-match-p "^#[0-9a-f]\\{6\\}$" color))
                          (when (string-match-p "^#[0-9a-f]\\{6\\}$" color)
                            (set-face-attribute asc:target-face nil :background color)))))
       (use-local-map widget-keymap)
       (widget-setup))

target:
001122
xxxxxx
