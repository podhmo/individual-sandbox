## go watch

真面目にwatchを考えるとめんどくさいな。。

- checker
- watcher
- releaser (applier)

## go go get

謎のエラー

```
go run 00hello/main.go
go: finding module for package github.com/go-chi/render
go: finding module for package github.com/go-chi/middleware
go: finding module for package github.com/go-chi/chi
go: found github.com/go-chi/chi in github.com/go-chi/chi v4.1.2+incompatible
go: found github.com/go-chi/render in github.com/go-chi/render v1.0.1
go: finding module for package github.com/go-chi/middleware
00hello/main.go:9:2: cannot find module providing package github.com/go-chi/middleware: module github.com/go-chi/middleware: git ls-remote -q origin in $HOME/vboxshare/sandbox/go/pkg/mod/cache/vcs/aec88034912d7397c519af999c5ce27e83cf181bd4246d3867202fd1713f9181: exit status 128:
        ERROR: Repository not found.
        fatal: Could not read from remote repository.

        Please make sure you have the correct access rights
        and the repository exists.
```
