# python json-reference json-pointerのライブラリについて調べる

- [gazpachoking/jsonref: Automatically dereferences JSON references within a JSON document.](https://github.com/gazpachoking/jsonref)
- [johnnoone/json-spec: Implements some tools for JSON](https://github.com/johnnoone/json-spec)

```
git clone --depth 1 git@github.com:johnnoone/json-spec.git
git clone --depth 1 git@github.com:gazpachoking/jsonref.git
```

- 色々なresolverのhookが使えるか調べる
- 外部resourceの参照が使えるか調べる
- 使いやすさ

# json-reference json-pointer について調べる

そもそもどれがどういうものか整理できていない。
これがデファクト感ある。

- [whitlockjc/json-refs: Various utilities for JSON Pointers and JSON References](https://github.com/whitlockjc/json-refs)

## json-reference

- [draft-pbryan-zyp-json-ref-03 - JSON Reference](https://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03)

外部リソースに対する参照を表すやつ `#/foo/bar/baz` とか `http://foo.bar.jp#foo/bar/baz` とか

## json-pointer

- [RFC 6901 - JavaScript Object Notation (JSON) Pointer](https://tools.ietf.org/html/rfc6901)

JSON document中の位置を表すもの

```
   The ABNF syntax of a JSON Pointer is:

      json-pointer    = *( "/" reference-token )
      reference-token = *( unescaped / escaped )
      unescaped       = %x00-2E / %x30-7D / %x7F-10FFFF
         ; %x2F ('/') and %x7E ('~') are excluded from 'unescaped'
      escaped         = "~" ( "0" / "1" )
        ; representing '~' and '/', respectively
```

以下の様なやつ。

```
"/foo/bar/boo/~0"
"/foo/bar/boo/~1"
"/foo/booz/0"
```

- "/"は"-"になる。
- 複合処理は `~1` を `/` に変換してから `~0` を `~` に変換する。 (`~01` が `~1` になることを保証)
- 配列の添字での `-` は配列の末尾の１つ先を表す(JSON-patchで使うもの)


## see-also

- [JSON PointerとJSON Patch - Qiita](http://qiita.com/taknuki/items/76d2fda912443b6854a4)

