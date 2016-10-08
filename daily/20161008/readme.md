# go urlのpathのjoin

example_urljoin

# python asyncioを使ってcrawlerを書く的なやつ

example_throttle

- 同一domainに対してだけwaittimeを設けたい
- 同一domainに対してもN個(e.g. 2個)だけは並行してrequestを投げたい
- 全体でのrequest数を制限したい

- crawlerなのでrequestの結果として更に探索対象のurlが見つかる

前者は基本的にsemaphoreを使えば良い。ただし時間辺りのOKみたいな処理は自分で書かなければいかなかった。
後者は未だに綺麗な形にならない。waitgroupのようなlockが必要なのだよなー。全部の処理が終わるということのチェックが辛い。
