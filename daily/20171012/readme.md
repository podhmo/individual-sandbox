## emacs shell-command-region

- `M-|` に shell-command-on-region がbindされている
- `C-u - 1 M-|` などするとcurrent-prefix-argに1が入る
- `C-u - 1 M-|` に `js -S .` を渡すとjsonのformatterとして使える

```lisp
(defun shell-command-on-region (start end command
				      &optional output-buffer replace
				      error-buffer display-error-buffer
				      region-noncontiguous-p)

  (interactive (let (string)
		 (unless (mark)
		   (user-error "The mark is not set now, so there is no region"))
		 ;; Do this before calling region-beginning
		 ;; and region-end, in case subprocess output
		 ;; relocates them while we are in the minibuffer.
		 (setq string (read-shell-command "Shell command on region: "))
		 ;; call-interactively recognizes region-beginning and
		 ;; region-end specially, leaving them in the history.
		 (list (region-beginning) (region-end)
		       string
		       current-prefix-arg
		       current-prefix-arg
		       shell-command-default-error-buffer
		       t
		       (region-noncontiguous-p))))
...
)
```


