# golang os.Openは `~` を認識しないっぽい

認識しないっぽい。以下の様なことをしたほうが無難だし。

```go
filename := strings.Replace("~/.bashrc", "~", os.GetEnv("HOME"))
fp, err := os.Open(filename)
```

真面目に対応するならこういうのを使ったほうが良さそうな感じ。

- https://github.com/mitchellh/go-homedir
