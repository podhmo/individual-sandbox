# オブジェクトの関連のちょっとしたメモを描きたい

[X](#X), [Y](#Y), [Z](#Z) というようなトップレベルのオブジェクトがある。

- オブジェクトの説明を書きたい
- オブジェクトの定義を表示したい
- オブジェクト間にリンクを付けたい
- (モジュールごとに表示したい)
- (逆参照のリンクを付けたい)
- (required/unrequiredを付与したい (not null?))
- (コードへのリンクを貼りたい)
- (親子関係、1:1,1:N,M:Nの関係を図示したい)
- 例を表示したい

## X

これはXです

| name | type | description |
| :--- | :--- | :--- |
| name | string! | 名前 |
| item | [Item](#Item) | アイテム |

examples <details>

```js
{
  "name": "foo",
  "item": {
    "name": "x
  }
}

{
  "name": "bar"
}
```

</details>

## Y

これはYです

| name | type | description |
| :--- | :--- | :--- |
| name | string! | 名前 |
| colorType | [Color](#Color)! | 色 |

## Z

これはZです

| name | type | description |
| :--- | :--- | :--- |
| name | string! | 名前 |
| itemList | [][Item](#Item) | アイテムリスト |

## Item

これはItemです

| name | type | description |
| :--- | :--- | :--- |
| name | string | 名前 |

## Color

これはenumです。

```
  Red   // 赤
| Blue  // 青
| Green // 緑
```
