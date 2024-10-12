# mermaidの図を画面いっぱいに表示したい

https://chatgpt.com/c/670a25ec-62f8-8001-8358-edef773e0515

## try01.html

とりあえず表示してみた。しかしマウスホイールでのズームとかが全然うごかないきがする。
(PC上ではCtrl + マウスホイールで拡大/縮小はできているようなきがする)

mermaid + panzoom.jsというので動いているみたい。
ChatGPTの返答は9.4.0を使うらしいけれど `Couldn't find the requested release version 9.4.0.`

https://github.com/mermaid-js/mermaid/issues/176

とりあえずversionを外せば動くみたい。あとはmermaidの初期化後に設定してもらわないと動かないようだ。
自動で読みこませると駄目みたい。mermaid.run()を利用しなくちゃいけなそう。そこでpostRenderCallbackを使う必要があるみたい。

動いたけれど一瞬mermaidに渡される内容が表示しちゃうのが嫌かも。
(元々はbodyなどの前にmermaidのコードが実行されていたから大丈夫だったかんじなようだった)

## try02.html

スマホに対応したものらしい。touchstartとかのイベントが追加されている

## try03.html

panzoomを取りのぞいたもの。svgがそのまま使われていた。そして意図したものではなかった。
たとえばsvg内の移動をしてほしかったがsvg要素が移動してしまう。



