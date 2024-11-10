# 複数のmapped typeのintersection typeへの代入について

以下の様な２つのmapped typeに値を代入する方法がわかっていなかったのだった。
TDefaultsはBooleansとStringsのintersectionで作られたmapped typeで、それぞれbooleanとstringを持つような型になっている。

デフォルト値が指定されていない場合に、negatablesで反転させることを考慮して埋めてあげたかった。
しかし代入の部分で以下のようなエラーが出る。

> [deno-ts] Element implicitly has an 'any' type because expression of type 'string' can't be used to index type '{ [K in Booleans[number]]?: boolean | undefined; } & { [K in Strings[number]]?: string | undefined; }'.
  No index signature with a parameter of type 'string' was found on type '{ [K in Booleans[number]]?: boolean | undefined; } & { [K in Strings[number]]?: string | undefined; }'.

こんなコード。

```ts
function F<
    Booleans extends readonly string[],
    Strings extends readonly string[],
    TDefaults extends
        & { [K in Booleans[number]]?: boolean }
        & { [K in Strings[number]]?: string },
>(
    options: {
        booleans: Booleans;
        strings: Strings;
        defaults: TDefaults;
        negatables?: Booleans;
    },
) {
    const values = { ...options.defaults };
    const negatables = options.negatables || [];
    (options.booleans || []).forEach((key) => {
        // ここで怒られる。Booleansであることが保証されたループなのだけれど
        if (values[key] === undefined) {
            values[key] = negatables.includes(key);
        }
    });
    return values;
}
```

## types1.ts

keyがstringとして扱われてBooleansとして扱われていないからエラーになるということはわかる。
`const booleanKey = key as Booleans[number];` こんなコードを書いてあげれば条件式のエラーは直せる。

asで変換しまくれば通せる感じにはなるけれどたぶん間違った方法なような気がする。

```diff
--- types0.ts
+++ types1.ts
@@ -10,11 +10,11 @@
         negatables?: Booleans,
     },
 ): { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string } {
-    const values = { ...options.defaults }
-    const negatables = options.negatables || [];
+    const values = { ...options.defaults } as { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string };
+    const negatables = (options.negatables || []) as Booleans;
     (options.booleans || []).forEach((key) => {
-        if (values[key] === undefined) {
-            values[key] = negatables.includes(key);
+        if (values[key as Booleans[number]] === undefined) {
+            (values as { [K in Booleans[number]]: boolean })[key as Booleans[number]] = negatables.includes(key);
         }
     });
     return values;
```

## types2.ts

どちらにしろあれ。。

```diff
--- types0.ts
+++ types2.ts
@@ -10,14 +10,14 @@
         negatables?: Booleans,
     },
 ): { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string } {
-    const values = { ...options.defaults }
-    const negatables = options.negatables || [];
+    const values = { ...options.defaults } as { [K in Booleans[number]]: boolean | undefined } & { [K in Strings[number]]?: string };
+    const negatables = (options.negatables || []) as Booleans;
     (options.booleans || []).forEach((key) => {
-        if (values[key] === undefined) {
-            values[key] = negatables.includes(key);
+        if (values[key as Booleans[number]] === undefined) {
+            (values as { [K in Booleans[number]]: boolean | undefined })[key as Booleans[number]] = negatables.includes(key);
         }
     });
-    return values;
+    return values as { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string };
 }
 
 const values = F({
```

## types2.ts

一度片側にキャストしてあとでキャストする。

```diff
--- types0.ts
+++ types3.ts
@@ -10,14 +10,14 @@
         negatables?: Booleans,
     },
 ): { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string } {
-    const values = { ...options.defaults }
-    const negatables = options.negatables || [];
-    (options.booleans || []).forEach((key) => {
+    const values = { ...options.defaults } as { [K in Booleans[number]]: boolean }
+    const negatables = (options.negatables || []) as Booleans;
+    (options.booleans || []).forEach((key: Booleans[number]) => {
         if (values[key] === undefined) {
             values[key] = negatables.includes(key);
         }
     });
-    return values;
+    return values as { [K in Booleans[number]]: boolean } & { [K in Strings[number]]?: string };
 }
 
 const values = F({
```