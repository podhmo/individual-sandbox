# python logging

structlogがだるいという話。

- 構造化ログとテキストログ
- contextual logger
- logging設定の遅延

## structlogがだるい

- fun-outっぽい機能ががない

遅延した設定が上手くいかないみたいな問題は解消されたっぽい(使い方の誤り?)

## loggingの設定が行われる前のログとかどうするべきなんだろ？

例えばアプリとかは以下の順になる。

1. mainの処理で色々ロード
1. アプリのconfigを見て設定(この時ログの設定が行われる)
1. アプリが実行される


この最初の段階でのログはどうするべきか？という話。

そもそもこの部分の出力にロガーを使うべきではない。またこの部分の状態はなるべく少なくするべきというのが答えっぽい。

## 構造化ログって

sructlog使わずともloggingの範疇で他の環境壊さずに実装できない？ -> できそう。


