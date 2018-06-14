#[python][csv][csvresumable]CSVを消費する処理を再開可能にしたい

CSVを消費する処理を再開可能にしたいという気持ちになりました。具体的には、１つ１つの処理にそこそこ時間が掛かる(30秒から1分)ものをそこそこ多く(10000件くらい)処理しないといけないことがあったのですが。DBとか用意したり使ったりするの面倒だなと思ったときのことです。

## CSVを消費したい(再開したい)

例えば以下の様なイメージです(実際の処理とは異なります)。

input.csv

```csv
id,x,y
1,10,20
2,100,100
```

このようなcsvがあって、これらの各行に対して処理を行う(例えば和を求める)のですが。途中で再開したいという感じです。

output.csv

```csv
id,v
1,30 // 本当は結構重たい
<-- このあたりで止めたい(再開したい)
2,200
```

実際、重いと言っても計算的なものではなく主に帯域制限的なものが原因です。なので並列化とかほぼ意味がない状態なのですが。途中で失敗したら辛いという感じになります。

随時終わるたびに書き出していき、終わったところまで入力を削るみたいな作業をしても良いのですが。だるい。

## csvresumable

まぁそんなわけでだるかったので。ちょっとした[ライブラリ](https://github.com/podhmo/csvresumable)を作ることにしました(まだ開発途中なのでAPIの変更は普通にあると思います)。具体的には以下の様な形で動きます。

- 通常のCSVのDictReaderと同様に動く
- どこまで終わったのかを別途記録する(history.csv)
- (再開時には、記録していたところまでの入力はスキップする)

状態など管理するのは面倒だったので、完全にinsertだけで済むようにしました。

例えば上の例で言えば、処理の途中で止めたいということは

```csv
id,x,y
1,10,20
<-- ここで止めたい
2,100,100
```

以下の様な履歴(history.csv)を用意し(csvである必要はない)

```csv
id
1
```

再開(resume)の際はこれとzipしたiterator(概念上)に対して処理を行えば良いということになります。

ちなみに、pandasなどのinterfaceを用意しなかった理由は、そもそも計算や集計が目的ではなかったためです。単なる情報をくれるevent streamとしてCSVがあれば良いというだけだったので(つまりCSVである理由も特にありません)。

## 実際の利用例

実際以下の様な形でかけます。

```python
import json
import time
import sys
from csvresumable import DictReader

with open("input.csv") as rf:
    r = DictReader(rf)
    for row in r:
        print("start", row["id"], file=sys.stderr)

        # 重たい処理
        time.sleep(2)
        print(json.dumps(row))
        sys.stdout.flush()
```

これで先程のinput.csv (再掲)

```csv
id,x,y
1,10,20
2,100,100
```

に対して実行して途中で(id=2の部分は計算が終わらない。あるいはエラー)止めます。

```console
$ python main.py
start 1
{"id": "1", "x": "10", "y": "20"}
start 2
^CTraceback (most recent call last):
  File "main.py", line 12, in <module>
    time.sleep(2)
KeyboardInterrupt
```

途中で止まったので、途中から再開したいはずです。ここで `RESUME=1` という環境変数と一緒に実行すると再開(resume)します。

```console
$ RESUME=1 python main.py
start 2
{"id": "2", "x": "100", "y": "100"}

# 全部実行し終わった後なら何も出力されない
$ RESUME=1 python main.py
```

ちなみにRESUMEをつけないとはじめからやり直しです。(つまり何も知らない人にとってはただのcsv.DictReaderとして動く)

```console
$ python main.py
start 1
{"id": "1", "x": "10", "y": "20"}
start 2
{"id": "2", "x": "100", "y": "100"}
```


### 環境変数以外の方法

ところで環境変数で設定というのが、設定より規約(CoC)っぽい感じがして嫌という人いると思います。そんな人は真面目にオプションを与えてください。以下の様な形で。

```diff
--- 00add/main.py	2018-06-14 17:45:23.000000000 +0900
+++ 01add/main.py	2018-06-14 18:36:18.000000000 +0900
@@ -1,10 +1,15 @@
 import json
 import time
 import sys
+import argparse
 from csvresumable import DictReader
 
+parser = argparse.ArgumentParser()
+parser.add_argument("--resume", action="store_true")
+args = parser.parse_args()
+
 with open("input.csv") as rf:
-    r = DictReader(rf)
+    r = DictReader(rf, resume=args.resume)
     for row in r:
         print("start", row["id"], file=sys.stderr)

```

`--resume` を使ってresumeできます

```console
$ python main.py
start 1
{"id": "1", "x": "10", "y": "20"}
start 2
^CTraceback (most recent call last):
  File "main.py", line 17, in <module>
    time.sleep(2)
KeyboardInterrupt

# resume
$ python main.py --resume
start 2
{"id": "2", "x": "100", "y": "100"}
```

## idとして扱う値を変えたい場合

## もう少し異なった形で消費したい場合

## 再開時に過去の出力を覚えておきたい場合

