## fastAPI, egoist

fastAPIで生成されるopenAPI docを眺めてみる。
validation errorは422で配列で返される。

これを良い感じにやるとしたらどうなるんだろうな？go-swaggerなどとの違いってなんなんだろう？
あんまり深煎りしたくなさがある。あー、そういう意味では全てに対応する気は無いのか。


## egoist

何をしようかな。
昨日はMakefileへの出力を作ってみたりしていた。
けっこう上手く動いていた。

直近気になったものはこのあたり

- submoduleを作ったときにsharedが使えない
- nestしたときに、上手くファイルが認識されるか
- jinja2はともかく、入力となるファイルのload時に元となるデータが必要になる？
- 複数のディレクトリに出力する処理をグルーピングしたい

その他思ったよりふつうに上手く動いてびっくりした。
