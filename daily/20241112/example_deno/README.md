# 型を詳細化したときのコードの書き方


個人的にはアプリケーションコードとツール用のコードと書き捨てのスクリプト用のコードで登る山が違うなーみたいな感覚がある。どれが良いかも一長一短な感じはあった。
たとえば、書き捨て用のスクリプトでは`Deno.*`的な関数を恐れず使うし、昨日やったYouTuneのAPIの実行結果の確認用のコードでは後半になりrespnseの型を頑張って付けるのはバカバカしくなった。

今回の話は型の話というよりは変数名などの名付けの話に近い。`const ob : Record<string, string>` からはじまって `& {color: Color}` のような型の詳細化を繰り返すようなコードをどういうふうに扱うか？というのが気になったりした。

- https://x.com/podhmo/status/1856306028029325481

## なにかを試行錯誤する前の記録のメモ

色々考えたりした結果は概ねChatGPTの最後の回等と同様の感じだった。

- https://chatgpt.com/c/6733446f-911c-8001-9b51-cc78ce502441

ここが本体。

> その状況における一般的な慣習や、読みやすさを保つための工夫についていくつか提案します。詳細化が進むときの命名やシャドーイングの使い方には工夫が求められますが、読みやすさを意識した以下のようなアプローチが考えられます。
> 
> ### 1. **明確な段階を表す命名規則を導入する**
>    変数名に「段階」を示すキーワードを付与し、何が追加されているかが分かるようにする方法です。
>    
>    例：
>    ```typescript
>    const baseOb = { name: "example", description: "A description" };
>    const obWithColor = { ...baseOb, color: "red" }; // 色情報が追加された
>    const obWithDetails = { ...obWithColor, extraInfo: "additional info" }; // さらに詳細情報を追加
>    ```
> 
>    このように、`WithColor` や `WithDetails` など、付加した要素に応じた名前を付けることで、読み手が各段階で何が追加されているか理解しやすくなります。
> 
> ### 2. **関数で詳細化処理を切り出す**
>    詳細化が複数ステップにわたる場合、それぞれを関数として分ける方法です。こうすることで、変数名の増殖を抑えつつ、読みやすさが向上します。
> 
>    例：
>    ```typescript
>    function addColor(ob: Record<string, string>): Record<string, string> & { color: Color } {
>        return { ...ob, color: "blue" };
>    }
>    
>    function addDetails(ob: Record<string, string> & { color: Color }) {
>        return { ...ob, extraInfo: "more details" };
>    }
> 
>    const ob = { name: "example", description: "A description" };
>    const detailedOb = addDetails(addColor(ob));
>    ```
> 
>    この方法なら、各詳細化のステップが独立した関数になり、変数名を多段階で増やさずに済みます。
> 
> ### 3. **一貫したシャドーイングを用いる**
>    シャドーイングを用いる場合も、統一的な書き方を意識することで可読性が向上します。各詳細化ステップをスコープ内にまとめ、型が詳細化されるごとに段階的に変数を更新する方法です。
> 
>    例：
>    ```typescript
>    {
>        const ob = { name: "example", description: "A description" };
>        {
>            const ob = { ...ob, color: "green" };
>            {
>                const ob = { ...ob, extraInfo: "additional details" };
>                console.log(ob);
>            }
>        }
>    }
>    ```
> 
>    この方法ではスコープを使って `ob` を徐々に詳細化していくため、関数での切り出しが不要な場合に有用です。ただし、スコープが深くなりすぎると読みづらくなるため、適度な段階にとどめることがポイントです。
> 
> ### 4. **型別のプレフィックスを使ったネーミング**
>    もし複数の詳細化パターンが多用される場合、プレフィックスとして型や目的を表す言葉（例えば `coloredOb` や `detailedOb` など）を使うと、変数名の付け方が一貫し、可読性が保たれます。
> 
> 最も適した方法は、コードの複雑さやコンテキストに依存しますが、いずれの手法も意図的に一貫性を持たせることで、読みやすさを確保できます。


## 実際にコードを書いてためしてみる

以前型ガード(type guard)を試したときに上手く絞り込まれていないような感じがした。（undefinedの対応が漏れていたのかもしれない、多分どちらかといえば自分の未熟の話）
とはいえ、型ガードでもifによる分岐でネストは増えていくような気がする？early returnのような形を取ればネストを浅くできるかもしれない。

### 型ガード


型ガードはearly returnのような形で書くときれいに書けるかもしれない。

```ts
// type-guardが使えるならまだマシ
function use2() {
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;

    if (!isColor(ob.color)) {
        throw new Error("invalid color");
    }
    if (!isDirection(ob.direction)) {
        throw new Error("invalid direction");
    }

    // [deno-ts] Type '{ readonly name: "foo"; readonly color: "red"; readonly direction: "north"; }' is not assignable to type 'never'.
    // const _ : never= ob

    console.dir(ob);
}
```

### シャドーイング


ところでシャドーイングの例で書かれたコードは動かないらしい。

```ts
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;
    {
        // [deno-ts] 'ob' implicitly has type 'any' because it does not have a type annotation and is referenced directly or indirectly in its own initializer.
        const ob = { ...ob, colors: "red" as Color } as const; // 
...
    }
```

### CPS

遊びでCPS。これはないと思う。あと渡すときにas unknownを経由するhackを使わないと型が合わなかったのが謎。

```ts
function use4() {
    const ob = {
        "name": "foo",
        "color": "red",
        "direction": "north",
    } as const satisfies Record<string, string>;

    contWithColor(ob, (ob) => {
        contWithDirection(ob, (ob) => {
            // [deno-ts] Type 'Omit<Omit<{ readonly name: "foo"; readonly color: "red"; readonly direction: "north"; }, "color"> & { color: "red" | "green" | "blue"; }, "direction"> & { direction: "north" | ... 2 more ... | "west"; }' is not assignable to type 'never'.
            // const _: never = ob;
            console.dir(ob);
        });
    });
}
```

ちなみにこのようなcontWithColor()のような関数を定義する段階で同じ問題が起きる。
そして型ガードがうまく効かない。

```ts
function contWithColor<
    T extends Record<string, string>,
    R extends Omit<T, "color"> & { color: Color },
>(
    ob: T,
    cont: (_: R) => unknown,
): unknown {
    if (ob.color === undefined) {
        throw new Error("color is required");
    }
    if (!isColor(ob.color)) {
        throw new Error("invalid color");
    }

    // Type 'string' is not assignable to type 'never'.
    // const _: never = ob.color; // ここでob.colorがColorになっていることを保証したい
    return cont(ob);
}
```

いっそのことas unknownを経由するhackを使った方が見通しが良い。

```ts
function contWithDirection<
    T extends Record<string, string>,
    R extends Omit<T, "direction"> & { direction: Direction },
>(
    ob: T,
    cont: (_: R) => unknown,
): unknown {
    if (!isDirection(ob.direction)) {
        throw new Error("invalid direction");
    }
    return cont(ob as unknown as R); // or  return cont({ ...ob, direction: ob.direction as Direction } as R);
}
```
