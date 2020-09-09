## go pkg/errors xerrors

そろそろ移行について考えたい

- https://github.com/pkg/errors/issues/202#issuecomment-559742099
- https://pkg.go.dev/errors

### 増えたメソッド

- As()
- Is()
- New()

### 代わりは？

- message付与 -- errors.Wrap() -> fmt.Errorf("%w", err)
- stacktrace付き -- errors.New() -> New()?
- 基底のエラーの確認 -- errors.Cause() -> Is()

### 裏側の話

- Unwrap()

### 確認したいこと

```
1. f pkg/errors.New()
2. f errors.New()
3. g pkg.errors.Wrap
  - f pkg/errors.New()
4. g fmt.Errorf
  - f pkg/errors.New()
5. g pkg.errors.Wrap
  - f errors.New()
6. g fmt.Errorf
  - f errors.New()
7. g fmt.Errorf
  - f fmt.Errorf("")
8. h pkg.errors.Wrap()
  - g.pkg.errors.Wrap()
    - f errors.New()
9. h pkg.errors.Wrap()
  - g. fmt.Errorf
    - f errors.New()
10. h fmt.Errorf
  - g. pkg.errors.Wrap()
    - f errors.New()
11. h fmt.Errorf
  - g. fmt.Errorf
    - f errors.New()
```

### 追記

もう少し単純に考えるか。
```
{a}, {b}
{a,a}, {a,b}, {b,a}, {b,b}
{a,a,a}, {a,b,a}, {b,a,a}, {b,b,a}, {a,a,b}, {a,b,b}, {b,a,b}, {b,b,b}
```

これくらい確認すれば良い。こういうのこそ、prestringで生成すれば良いのか。
考えてみると以下の使い分けもあった。今まで気にしてなかったけど。

- WithMessage()
- Wrap()

本当はWrap()は一回だけで良かった。まぁソレは置いておいて。

イテレーターに話を向けると、外部イテレーターと内部イテレーターとあったりしますが。

あ、そうそう。
RESTというのは、web APIを作るときにこういう形でendpointの名前や設計をすると綺麗に書けるよというお作法の名前だったりします。

タイミング、頻度、データ量、接続数などによって変わってきそうですね。

    
