# typescriptの型を理解したい

https://chatgpt.com/share/671e08cd-0da0-8001-95f6-eee85ced2e77 

以下のような質問から始めて理解をしていこうと思った

> TypeScriptを書いてます。型パズルについての相談に乗ってください。
> 
> 例えばある関数Fを考えます。このFはオブジェクトと文字列の配列を引数に取ります。ここでオブジェクトは任意のキーを持つことができこの値はstring |undefinedです。また2つ目の引数はrequiredという名前になっておりここに存在する文字列に対応する名前の値が戻り値ではstringに絞られることを期待してます。このような関数を書くときに型はどのように書けば良いでしょう？

大体わかったので類題に挙げられている問題を解いてみることにする

## 思ったこと

- 類題3でobjectに含まれない値を許す方法がわからなかった
- `KS[number]` みたいな記述を使うことがなかった -> `K extends (typeof T)[]` みたいにして無理やり使えなくもない
- 便利な型宣言を使いこなせてない (e.g. Omit)

## 参考

- https://www.typescriptlang.org/docs/handbook/2/types-from-types.html
- https://www.typescriptlang.org/docs/handbook/utility-types.html