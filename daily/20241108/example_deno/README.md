# typescriptのまだ知らない機能を使ってみる

## const type parameters

### array

 これはもしかして今まで必要になってきたas const assertionが不要になるのでは?

 いけそう(./const_type_parameters.ts)。

 ```ts
 function use2<const T extends string[]>(xs: T): { xs: T } {
    return { xs: xs }
}
 ```

 気になるのは以前必要になっていたEnforceLiteralArrayみたいなユーティリティも不要になるかも？ということ
 あと、`T[number][]`みたいな謎の作業も不要になっている？

### object

optionsでも上手くいくんだろうか？

この引数がundefinedのときはどうだろう？

### [] or undefined or ["x", "y"]

実際の利用方法はパラメーターがが省略可能になっている。その辺を含めるとAssertLiteralType()は消せなかった。
渡される可能性のあるものはこの省略されたときにwideingが発生しがちだった。

- 省略      - - ["x", "y", "z"], undefined
- 明示的に空  -- ["x", "y", "z"], []
- subset     -- ["x", "y", "z"], ["x"]
- not subset -- ["x", "y", "z"], ["i"]

ついでにtypeofでparameterを取り出す必要があった。これで上手くいきそう。

 # 参考

 - https://speakerdeck.com/nearme_tech/typescript-5-dot-0-const-xing-parametanoshi-idao
