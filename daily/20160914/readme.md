# shell 手抜きのreplace

```
function replace () { grep -lr $1 . --exclude=.git | xargs gsed -i "s@$1@$2@g"; }
function replace2 () { echo "grep -lr \"$1\" .  --exclude=.git | xargs gsed -i \"s@$1@$2@g\";"; }
```

# golang 値のcopyについて

代入でもコピーが起きる。

```go
type Point struct {
	X, Y int
}

a := Point{X: 10, Y: 20}
b := a
a.X = 100
// b.X is 10
```

だからloopでclosureを使う時に代入を書いているのではないか。

```go
for i := 0; i<3; i++ {
    i := i
    r = append(r, func(){ fmt.Println(i) })
}
```

# mac applicationの uninstall

```
rm -rf /Application/<app>
rm -rf ~/Library/<app>
```
