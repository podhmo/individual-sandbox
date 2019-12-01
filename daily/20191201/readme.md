## go-cloud potable type constructor

https://github.com/google/go-cloud/blob/de3c1e52abcb90518a0f64cd75f01a8db1072982/internal/docs/design.md#portable-type-constructors

そもそも何を言っているのか。公開用の型をinterfaceではなくconcreate typeにしているという話。

ちなみにそれぞれのリンク先の型の実装を見ると以下の様になっている。

- 確かにconcreate type
- 全てのfieldがprivate
- メソッドが定義されている

そもそもなんで？というのが気になる点。思ったのは以下。（確証はない）

interfaceの場合

```
app -> blob.bucket[I] <-- awsblob.Bucket[C]
```


- interfaceの定義自身が型などをかなり厳密に規定してしまう。
- そして共通部分をinterfaceにしてしまうとそれそのものを全部実装する必要がある
- スケールしない
- 例えばrubyのEnumerable moduleを使ったmixinと似たようなもののEnumerale的なものはconcrete typeでみたいな
- あるいはinterfaceの利用のお気持ちは高階関数に渡される方の関数 https://mobile.twitter.com/podhmo/status/1197747857152856066

参考にしているのはdatabase/sql。

> Instead, we follow the example of database/sql and separate out the implementation-agnostic logic from the interface. The implementation-agnostic logic-containing concrete type is the portable type. We call the interface the driver. Visually, it looks like this:

```
app -> blob.Bucket[C] <>- driver.Bucket[I] <-- awsblob.Bucket[C]
```

### 具体例

blob.Bucket

```go
// Bucket provides an easy and portable way to interact with blobs
// within a "bucket", including read, write, and list operations.
// To create a Bucket, use constructors found in driver subpackages.
type Bucket struct {
	b      driver.Bucket
	tracer *oc.Tracer

	// mu protects the closed variable.
	// Read locks are kept to allow holding a read lock for long-running calls,
	// and thereby prevent closing until a call finishes.
	mu     sync.RWMutex
	closed bool
}
```

runtime.Variable

```go
// Variable provides an easy and portable way to watch runtime configuration
// variables. To create a Variable, use constructors found in driver subpackages.
type Variable struct {
	dw       driver.Watcher
	provider string // for metric collection; refers to driver package name

	// For cancelling the background goroutine, and noticing when it has exited.
	backgroundCancel context.CancelFunc
	backgroundDone   chan struct{}

	// haveGoodCh is closed when we get the first good value for the variable.
	haveGoodCh chan struct{}
	// A reference to changed at the last time Watch was called.
	// Not protected by mu because it's only referenced in Watch, which is not
	// supposed to be called from multiple goroutines.
	lastWatch <-chan struct{}

	mu       sync.RWMutex
	changed  chan struct{} // closed when changing any of the other variables and replaced with a new channel
	last     Snapshot
	lastErr  error
	lastGood Snapshot
}
```

### もう少し

portable typeだけを見てもわかんないな。。

- portable type https://github.com/google/go-cloud/blob/0dde1a6ec889d28aeab0d8bd36bce4b6d553b62a/blob/blob.go#L437
- driver https://github.com/google/go-cloud/blob/a4f4e498fa3c4c4eb867a89d06c5f82bc1c1a7f1/blob/driver/driver.go#L223
- options (implementation) https://github.com/google/go-cloud/blob/a4f4e498fa3c4c4eb867a89d06c5f82bc1c1a7f1/blob/memblob/memblob.go#L97

うーん。キモはこの辺りかも。

blob

https://github.com/google/go-cloud/blob/0dde1a6ec889d28aeab0d8bd36bce4b6d553b62a/blob/blob.go#L477-L492

```go
// NewBucket is intended for use by provider implementations.
var NewBucket = newBucket
```

implementation

https://github.com/google/go-cloud/blob/a4f4e498fa3c4c4eb867a89d06c5f82bc1c1a7f1/blob/memblob/memblob.go#L90-L100

```go
// OpenBucket creates a *blob.Bucket backed by memory.
func OpenBucket(opts *Options) *blob.Bucket {
	return blob.NewBucket(openBucket(opts))
}
```

実際の使いかた

https://gocloud.dev/howto/blob/#opening

実際にはコレが使われるのか。

https://github.com/google/go-cloud/blob/0dde1a6ec889d28aeab0d8bd36bce4b6d553b62a/blob/blob.go#L1088-L1090

```go
func OpenBucket(ctx context.Context, urlstr string) (*Bucket, error) {
	return defaultURLMux.OpenBucket(ctx, urlstr)
}

// OpenBucket calls OpenBucketURL with the URL parsed from urlstr.
// OpenBucket is safe to call from multiple goroutines.
func (mux *URLMux) OpenBucket(ctx context.Context, urlstr string) (*Bucket, error) {
	opener, u, err := mux.schemes.FromString("Bucket", urlstr)
	if err != nil {
		return nil, err
	}
	return applyPrefixParam(ctx, opener.(BucketURLOpener), u)
}
```

mapに保持しているのか。packageの依存は全部載せっぽい。

```go
// The zero value is a multiplexer with no registered schemes.
type URLMux struct {
	schemes openurl.SchemeMap
}
// RegisterBucket registers the opener with the given scheme. If an opener
// already exists for the scheme, RegisterBucket panics.
func (mux *URLMux) RegisterBucket(scheme string, opener BucketURLOpener) {
	mux.schemes.Register("blob", "Bucket", scheme, opener)
}
```

素直にinitで登録しているのか。。

https://github.com/google/go-cloud/blob/a4f4e498fa3c4c4eb867a89d06c5f82bc1c1a7f1/blob/memblob/memblob.go#L56-L62

```go
func init() {
	blob.DefaultURLMux().RegisterBucket(Scheme, &URLOpener{})
}

// Scheme is the URL scheme memblob registers its URLOpener under on
// blob.DefaultMux.
const Scheme = "mem"
```

## 気になる

こういうのURL列挙してても仕方がない気がするな。。

- https://ritou.hatenablog.com/entry/2019/12/01/060000
- https://speakerdeck.com/player/eab79ccf11154667812a827ad392481e
- https://chrispenner.ca/posts/wc
- https://qiita.com/koinori/items/6829403841487f1cc440
- https://qiita.com/kripkejoyal/items/6f70c794e6f11a2340f1
- https://qiita.com/dcm_chida/items/0b687fe42b932e090a36
- https://squidfunk.github.io/mkdocs-material/
- https://qiita.com/advent-calendar/2019/yarakashi-production
- https://readingmonkey.blog.fc2.com/blog-entry-684.html

