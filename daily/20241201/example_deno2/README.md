# TypeScript/JavaScriptのSymbolについて理解したい

背景：CLIのコマンドライン引数を解析した結果の値から元々のオプション設定を取り出したい。
しかし、なんとなくの気持ちとして返す値をオブジェクトにしてメソッドを持たせたくない。

- JSON.strigifyの対象外になってほしい
- 補完の対象外になってほしい？（あるいは補完の対象になってほしい）

ということで`Record<string | Symbol, unknown>`のときのSymbolの扱いがどうなっているのかを把握できていなかった。

## JSON.stringifyなど

色々試してみる

```ts
const foo = {
    name: "foo",
    age: 10,
    sayHello: function () {
        console.log(`Hello, I'm ${this.name}`);
    },
    [Symbol("help")]: "hoi",
};

{
    const ob = foo;
    console.log("json");
    console.log(JSON.stringify(ob)); // {"name":"foo","age":10}
    console.log("----------------------------------------");
    console.log("console.dir");
    console.dir(ob, { depth: null }); // { name: 'foo', age: 10, sayHello: [Function (anonymous)], [Symbol(help)]: 'hoi' }
}
```

どうやら

- JSONの対象にはならない感じらしい。
- スプレッドで値を作り直したときも残るらしい

しかし、値はどうやって取り出すのだろう？

## 値を取り出したい

作った値を元に取り出す感じっぽい。unique identifier的な感じ？

```ts
const bar = Symbol("bar");

const foo = {
    [Symbol("foo")]: "foo",
    [bar]: "bar",
};

console.log("ob[Symbol('foo')]", foo[Symbol("foo")]); // undefined
console.log("ob[bar]", foo[bar]); // bar
```


## references

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol