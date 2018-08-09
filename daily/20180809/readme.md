## emacs side windows

どうもside windowsの設定がdefaultになったのでdiredで開いた時の設定が既存と異なるという感じっぽい(from 26.1)。

- https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Side-Windows.html
- https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Frame-Layouts-with-Side-Windows.html#Frame-Layouts-with-Side-Windows

```lisp
(defvar parameters
  '(window-parameters . ((no-other-window . t) (no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

(defun dired-default-directory-on-left ()
  "Display `default-directory' in side window on left, hiding details."
  (interactive)
  (let ((buffer (dired-noselect default-directory)))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . fit-window-to-buffer)
              (preserve-size . (t . nil)) ,parameters)))
)
;; (window-list)
```


`C-x 1` とかで消えないwindowが作れるっぽい？

