# golang GOPATHとGOROOTを見つける方法

shell上から

```
$ go env | grep -P 'GOPATH|GOROOT'
GOPATH="/home/<me>/go"
GOROOT="/opt/local/lib/go"
```

goのコード上

```
$ gore
gore version 0.2.6  :help for help
gore> :import os
gore> os.Getenv("GOPATH")
"/Users/me/go"
gore> :import runtime
gore> runtime.GOROOT()
"/opt/local/lib/go"
```
