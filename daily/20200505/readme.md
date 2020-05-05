## miniconfigのメンテナンス

- https://github.com/podhmo/miniconfig

打ち捨てられたリポジトリをもう一度触ろうとしたときにけっこうやることがあるな

- Makefileの追加
- README.rst -> README.md
- setup.cfgを追加
- tranvis.ymlを更新
- blackをかける
- lintを治す
- warningを治す

## egoist structを生成

昨日のやつはだいぶ良くなった [../20200504/readme.md](../20200504/readme.md)

ただ、関数として用意しているだけではダメで良い感じのlinker的な操作ができる必要がありそう。
これと、既存のclikitを良い感じに調和させようとするにはどうすれば良いかなどを考えたりしていた。

けっこういろいろ考えていたけれど。結局egoistとマージするにはapp化しないとダメそう。

そんなわけで、miniconfigを治そうという気になった。

