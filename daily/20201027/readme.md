## permissionの例ないかな？

- https://docs.laminas.dev/laminas-permissions-acl/usage/

ACL

| Name  | Unique Permissions  | Inherit Permissions From |
| :--- | :--- | :--- |
| Guest  | View  | N/A |
| Staff  | Edit, Submit, Revise  | Guest |
| Editor  | Publish, Archive, Delete  | Staff |
| Administrator  | (Granted all access)  | N/A |


## emacs eglot

どういう単位でgroupingされるんだろう？

```
(insert (cdr (project-current)))
;; ~/vboxshare/venvs/my/individual-sandbox/

(eglot--guess-contact nil)
(markdown-mode (vc . "~/vboxshare/venvs/my/individual-sandbox/") eglot-lsp-server ("efm-langserver" "-logfile" "/tmp/efm.log" "-loglevel" "10"))
```

eglotはproject-currentごとか。。

