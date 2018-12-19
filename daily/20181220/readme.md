## go 小さなinterfaceを大きなinterfaceに持ち上げる

- 気持ちはrubyのEnumerable
- coreな関数とそれに依存した関数群
- loggerとかそう？(https://www.kaoriya.net/blog/2018/12/16/)

## go logのオプション

以下の呼び出し

```
log.Printf("hello")
```

以下になる(log.go)

```
std.Output(2, fmt.Sprintf("hello"))
```

ここで

```
var std = New(os.Stderr, "", LstdFlags)
```

ちなみにOutput()は、以下のときにruntime.Callerを呼んで教えてくれる

```go
l.flag&(Lshortfile|Llongfile) != 0 
```

利用できるflagは以下

```
const (
	Ldate         = 1 << iota     // the date in the local time zone: 2009/01/23
	Ltime                         // the time in the local time zone: 01:23:23
	Lmicroseconds                 // microsecond resolution: 01:23:23.123123.  assumes Ltime.
	Llongfile                     // full file name and line number: /a/b/c/d.go:23
	Lshortfile                    // final file name element and line number: d.go:23. overrides Llongfile
	LUTC                          // if Ldate or Ltime is set, use UTC rather than the local time zone
	LstdFlags     = Ldate | Ltime // initial values for the standard logger
)
```

## go loggerの良い使い方

利用者が差し替え可能な形に

```go
var Logger *log.Logger
```

もう少し丁寧にやるなら

```go
var (
	logger = log.New(os.Stderr, "", log.LstdFlag)
    logMu sync.Mutex
)

func SetLogger(l *log.Logger)
```

さらにこれをinterfaceに

