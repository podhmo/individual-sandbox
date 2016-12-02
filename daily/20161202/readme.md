# emacs golang gorimports をgitでmodifiedなファイル実行したい

現在のプロジェクトしたにある変更済みのファイルに対して `goimports` を実行したい。

```lisp
;; need pickup-file
(defun my:go-pickup-project-directory-path ()
  (pickup-file ".git"))

(defun my:go-gofmt-modified-buffers-async ()
  (interactive)
  (let* ((project-path (file-name-directory (my:go-pickup-project-directory-path)))
         (tmp-buf (generate-new-buffer " *go-modified-file*"))
         (cmd (format "git ls-files -m %s | grep -v '/vendor/' | xargs goimports -w" project-path)))
    (lexical-let ((tmp-buf tmp-buf) (cmd cmd))
      (set-process-sentinel
       (start-process-shell-command "go-gofmt-modified-buffers-async" tmp-buf cmd)
       (lambda (sig status &rest args)
         (condition-case nil
             (progn
               (message (format "%s: %s" (replace-regexp-in-string "\n$" "" status) cmd))
               (kill-buffer tmp-buf))
           ((error) (display-buffer tmp-buf))))))))
```

`C-x C-s` の `save-buffer` に対して `gofmt(goimports)` をbindしているので。
`C-x S` の `save-some-buffers` に対して これをbindするのが良さそう。
