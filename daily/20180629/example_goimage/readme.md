## gcc search dirを探す

```
$ gcc -print-search-dirs
```

## python 久しぶりにctypesに親しむ

- http://yizhang82.me/python-interop-inside-ctypes

- structを返す時どうなるんだっけ？

## go shared libraryを出力

```
go build -o <libxxx>.so --buildmode c-shared <libxxx>.go
```

ここで <libxxx>.go は以下のようなもの

```go

// F :
//export f
func F(name *C.char) C.int {
	name := C.GoString(name)
    return c.int(len(name))
}
```

- https://github.com/vladimirvivien/go-cshared-examples
