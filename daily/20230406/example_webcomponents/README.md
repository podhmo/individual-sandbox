# web componentsをいじりたくなった

- [カスタム要素の使用 - ウェブコンポーネント | MDN](https://developer.mozilla.org/ja/docs/Web/Web_Components/Using_custom_elements)
    -  https://github.com/mdn/web-components-examples/blob/main/popup-info-box-web-component/main.js
- [Card: Spectrum Web Components](https://opensource.adobe.com/spectrum-web-components/components/card/)
    - esm.shでいい感じに設定したい
    - `import '@spectrum-web-components/card/sp-card.js';` side effect
- とりあえずカード要素を


参考

* [ブログカードを支える技術 - Qiita](https://qiita.com/hinastory/items/05baa8aec64a75026b3a)
* [カードデザインのポイントと実装方法 | Webクリエイターボックス](https://www.webcreatorbox.com/tech/card)
* [外部サイトのOGPを取得する](https://zenn.dev/littleforest/articles/scrape-og-tags)
* [Card: Spectrum Web Components](https://opensource.adobe.com/spectrum-web-components/components/card/)
* [とりあえず実装してみるWeb Components](https://zenn.dev/yend724/articles/20220315-7yr5v8hecfl085nm)
* [Cards – Material Design 3](https://m3.material.io/components/cards/overview)


- webcompnentsを理解
  - ok 一度それっぽく表示させる
  - ok htmlを文字列で渡すようにする
  - ok 内部の要素をそのまま表示させるようにする
  - ok cssを適用するようにする
  - skip constructorとrenderを分ける
  - ok (shadow domのmode openの意味 -- 外部からアクセス可能 (e.g. js))
  - ok card-listを利用して表示
  - .card-list > .card の表示をうまくやる

内部の様をそのまま表示  

- [テンプレートとスロットの使用 - ウェブコンポーネント | MDN](https://developer.mozilla.org/ja/docs/Web/Web_Components/Using_templates_and_slots)
  - slotを指定せずにchildrenのようなものはない？
- [Web components の使い方/作り方（ウェブコンポーネント入門）](https://www.webdesignleaves.com/pr/jquery/web-components-basic.html)
  - default slotでは？ `<slot></slot>`
- [::slotted() - CSS: Cascading Style Sheets | MDN](https://developer.mozilla.org/en-US/docs/Web/CSS/::slotted)
  - 子孫ノードを触るのは `::slotted(*)`