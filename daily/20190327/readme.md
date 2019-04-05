# emacs ivy

ivy-postframe

https://github.com/tumashu/ivy-posframe

これを設定してあげるとdisplay-functionをその場で表示できる様になるっぽい。

```lisp
  (use-package ivy-posframe
    :ensure t
    :config
    (setq ivy-display-function nil) ; default
    (add-to-list 'ivy-display-functions-alist '(complete-symbol . ivy-posframe-display-at-point))
    ;; (ivy-mode)
    (ivy-posframe-enable))
```

help messageを併せて表示できるようにならないかな。

completion-extra-properties is a variable defined in ‘minibuffer.el’.

https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-Variables.html

この辺使えないかな。

```
‘:annotation-function’: Function to annotate the completions buffer.
   The function must accept one argument, a completion string,
   and return either nil or a string which is to be displayed
   next to the completion (but which is not part of the
   completion).  The function can access the completion data via
   ‘minibuffer-completion-table’ and related variables.
```

http://garethrees.org/2015/02/09/emacs/

## ivy

- ivy-format-function

```
(let ((ivy-format-function #'ivy-format-function-arrow)
      (ivy-display-function #'ivy-posframe-display-at-point))
  (ivy-read "> " '("foo" "bar") :action #'insert)
  )
```

hmm

```lisp
(defun ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "> " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))
```

ivy-wrap?
ivy-height 20

https://github.com/abo-abo/swiper/issues/549
