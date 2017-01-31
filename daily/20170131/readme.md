# mac excel インストーラーが "検証中" のまま先に進まない。

以下の様にして検証自体を無効にしてあげると立ち上がる様になる。

```bash
$ xattr -dr com.apple.quarantine <app name>.pkg
```



