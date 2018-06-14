#[python][csv][csvresumable]pythonでCSVを消費する処理を再開可能にしたい

[https://github.com/podhmo/csvresumable:embed:cite]

## はじめに

CSVを消費する処理を再開可能にしたいという気持ちになりました。具体的には、１つ１つの処理にそこそこ時間が掛かる(30秒から1分)ものをそこそこ多く(10^4件くらい)処理しないといけないことがあったのですが。DBとか用意したり使ったりするの面倒だなと思ったときのことです。

## CSVを消費したい(再開したい)

例えば以下の様なイメージです(実際の処理とは異なります)。

input.csv

```csv
id,x,y
1,10,20
2,100,100
```

このようなcsvがあって、これらの各行に対して処理を行う(例えば和を求める)必要があるとします。

output.csv

```csv
id,v
1,30 // 本当は結構重たい
<-- このあたりで止めたい(再開したい)
2,200
```

実際、重いと言っても計算的なものではなく主に帯域制限的なものが原因です。なので並列化とかほぼ意味がない状態なのですが。途中で失敗したら辛いという感じの状況です。

随時終わるたびに書き出していき、終わったところまで入力を削るみたいな作業をして手動で入力となるファイルを書き換えていっても良いのですが。だるい。

## csvresumable

まぁそんなわけでだるかったので。ちょっとした[ライブラリ](https://github.com/podhmo/csvresumable)を作ることにしました(まだ開発途中なのでAPIの変更は普通にあると思います)。具体的には以下の様な形で動きます。

- 通常のCSVのDictReaderと同様に動く
- どこまで終わったのかを別途記録する(history.csv(実際には別の場所に記録されます))
- (再開時には、記録していたところまでの入力はスキップする)

状態などを管理するのは面倒だったので、完全にinsertだけで済むようにしました。

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

再開(resume)の際はこれとzipしたiterator(概念上)に対して処理を行えば良いということになります。履歴に残ったものはスキップしてしまえば良いということです。

ちなみに、pandasなどのinterfaceを用意しなかった理由は、そもそも計算や集計が目的ではなかったためです。単なる情報をくれるevent streamとしてCSVがあれば良いというだけだったので(つまりCSVである理由も特にない可能性があります。そのあたりも込みでinterfaceが変わりうるという感じです)。

## 実際の利用例

以下の様な形で書けます。`csv.DictReader`のかわりに`csvresumable.DictReader` を使います。

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
        print(json.dumps(row))  # 重たい処理をした結果のつもり
        sys.stdout.flush()
```

CSVから１行ずつ取得していき、それを入力として何らかの処理を行うという形です。この処理がそこそこ重めの処理(と言っても先程言った通りにほぼほぼ流量制限が原因で高速化できない)になっており、数十秒程度掛かると言ったものだとします。

例えば先程のinput.csv (再掲)に対して実行し、

```csv
id,x,y
1,10,20
2,100,100
```

実行を途中で止めます(id=2の部分は計算が終わらない。あるいはエラー)。

```console
$ python main.py
start 1
{"id": "1", "x": "10", "y": "20"}
start 2
	KeyboardInterrupt
```

途中で止まったので、途中から再開したいはずです。ここで `RESUME=1` という環境変数と一緒に実行すると再開(resume)できます。

```console
$ RESUME=1 python main.py
start 2
{"id": "2", "x": "100", "y": "100"}

# 全部実行し終わった後なら何も出力されない
$ RESUME=1 python main.py
```

ちなみにRESUMEをつけないとはじめからやり直しです(つまり何も知らない人にとってはただのcsv.DictReaderとして動く)。

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
	KeyboardInterrupt

# resume
$ python main.py --resume
start 2
{"id": "2", "x": "100", "y": "100"}
```

## idとして扱う値を変えたい場合

idとして使う値を変えたくなることがあるかもしれません。その場合には`key`オプションがあります。これは`sorted()`関数と同様のイメージで考えてもらえれば良いです。渡されるCSVというのはかならずしも常に自分の意図した通りの構造で渡されるということがなかったりしますし。

例えば、以下の様な形かもしれません。

```csv
groupId,userId,name,age,cache
1,1,foo,8,1000
1,2,bar,10,200
2,3,boo,2,0
3,4,bar,2,100
```

デフォルトでは左端をidとして扱いますが、上の例では左端のgroupIdではなくuserIdをidとして消費したくなると思います。このような場合には以下の様に書けば良いです。

```python
import time
import sys
from csvresumable import DictReader

with open("input.csv") as rf:
    r = DictReader(rf, key=lambda row: row["userId"])  # key=を使う
    for row in r:
        print("start", row["userId"], file=sys.stderr)

        # 重たい処理
        time.sleep(2)
        print(row["name"], int(row["age"]) / int(row["cache"]))
        sys.stdout.flush()
```

テキトウに年齢(勤続年数?)を貯金で割って、1万円？を稼ぐのに何年掛かるのかというような値でも計算してみましょう(これはテキトーな例です)。

```console
python main.py
start 1
foo 0.008
start 2
bar 0.05
start 3
Traceback (most recent call last):
  File "main.py", line 12, in <module>
    print(row["name"], int(row["age"]) / int(row["cache"]))
ZeroDivisionError: division by zero
```

おや、エラーになってしまいましたね。0除算を気にしてませんでした(まぁこういう感じでたまに考慮漏れのエラーがあったりします)。
テキトウに直したら。

```diff
--- 02groupid/main.py	2018-06-15 01:13:54.785744154 +0900
+++ 03groupid/main.py	2018-06-15 01:22:14.473955859 +0900
@@ -9,5 +9,9 @@
 
         # 重たい処理
         time.sleep(2)
-        print(row["name"], int(row["age"]) / int(row["cache"]))
+        if int(row["cache"]) == 0:
+            ans = "-"
+        else:
+            ans = int(row["age"]) / int(row["cache"])
+        print(row["name"], ans)
         sys.stdout.flush()
```

再開します。

```console
$ RESUME=1 python main.py
start 3
boo -
start 4
bar 0.02
```

途中から再開できてますね。

## 複数のCSVを合成した結果を元に消費したい場合

ところで、今までは入力がひとつだけでしたが。複数の入力が必要になることもあると思います。そのような場合にも一応対応はしています。

### 直列につなぐ場合(concat)

単純に複数に分割されたファイルを入力だとしましょう。

input.csv

```csv
groupId,userId,name,age,cache
1,1,foo,8,1000
1,2,bar,10,200
```

input2.csv

```csv
groupId,userId,name,age,cache
2,3,boo,2,0
3,4,bar,2,100
```

そのような場合はつなげるだけです。

```python
import time
import sys
from csvresumable import DictReader

# １つではなく２つなのでforループ
for filename in ["input.csv", "input2.csv"]:
    with open(filename) as rf:
        r = DictReader(rf, key=lambda row: row["userId"])
        for row in r:
            print("start", row["userId"], file=sys.stderr)

            # 重たい処理
            time.sleep(2)
            if int(row["cache"]) == 0:
                ans = "-"
            else:
                ans = int(row["age"]) / int(row["cache"])
            print(row["name"], ans)
            sys.stdout.flush()
```

注意点としてはDictReaderのiteratorをリストなどにして消費しないようにしてください。

```
$ python main.py
start 1
foo 0.008
start 2
	KeyboardInterrupt
$ RESUME=1 python main.py
start 2
bar 0.05
start 3
boo -
start 4
	KeyboardInterrupt
$ RESUME=1 python main.py
start 4
bar 0.02
```

ファイルの切れ目など関係なくresumeできています。これは当然と言えば当然なのですが。渡すファイルの順序は常に一定にしてください(あるときは `input2.csv input.csv` などの順序であるなど順序が不定の場合にはおかしくなります)。

### 並列につなぐ場合(groupby)

今度は並列につなぐ場合を考えてみます。例えば先程のcsvについてgroupIdでgroupingされた結果に対する何らかの処理をしてみるということにしてみましょう。そのような場合でも考え方は同様です。毎回常に一定の順序でeventが発生するevent streamのようなものが構成されていればそれで十分です(入力がCSVである必要も特にありません)。

このような場合には `csvresumable.iterate` を使います。

```python
import time
import sys
import csv
import itertools
import csvresumable

# event streamはiteratorであれば良い
def gen(files):
    sources = [csv.DictReader(open(f)) for f in files]
    sorted_sources = sorted(itertools.chain.from_iterable(sources), key=lambda row: row["groupId"])
    return itertools.groupby(sorted_sources, key=lambda row: row["groupId"])


for group_id, rows in csvresumable.iterate(gen(["input.csv", "input2.csv"])):
    print("start group_id", group_id, file=sys.stderr)
    time.sleep(2)
    print("total", sum(int(row["cache"]) for row in rows))

# groupingは以下の様な形
# 1 [{"groupId": "1", "userId": "1", "name": "foo", "age": "8", "cache": "1000"},
#    {"groupId": "1", "userId": "2", "name": "bar", "age": "10", "cache": "200"}
#   ]
# 2 [{"groupId": "2", "userId": "3", "name": "boo", "age": "2", "cache": "0"}]
# 3 [{"groupId": "3", "userId": "4", "name": "bar", "age": "2", "cache": "100"}]
```

例を見てわかる通り、実は入力がCSVである必要はありません。一定の順序を保った何らかのeventのstreamであれば大丈夫です(pythonで言えばiterator)。
defaultでは`itertools.groupby`などと同様にiterateされた行をリストと捉えての最初の要素をidとして扱いますが(`key=lambda xs: xs[0]`)、もちろんkeyオプションがとれます。

chainしてsortしてとやっているので、原理的には与えられたファイルを全部見ているわけですが。そもそも冒頭で触れたように元となる入力の数自体はせいぜい10^4程度しかありません。なのでそこまでコストというわけでもないです。

途中で止めてRESUMEで再開できます。

```console
$ python main.py
start group_id 1
total 1200
	KeyboardInterrupt

$ RESUME=1 python main.py
start group_id 2
total 0
start group_id 3
total 100
```

### おまけ

ちなみにchainしてsortしてgroupbyというのは結構よくやる処理なのですが。毎回書くのもめんどくさいのでconcat_groupbyという関数を用意しています。

```python
def gen(files):
    source = [csv.DictReader(open(f)) for f in files]
    return csvresumable.concat_groupby(source, key=lambda row: row["groupId"])
```

ところで先頭N件だけ取りたいという場合にはitertools.isliceが使えます。

```python
def gen(files, *, size=None):
    sources = [csv.DictReader(open(f)) for f in files]
    itr = csvresumable.concat_groupby(sources, key=lambda row: row["groupId"])
    if size is not None:
        itr = itertools.islice(itr, size)
    return itr
```

対象となるevent streamは消費しないように気をつけてください(消費というのは`list(gen(files))`のようなことを指してます)。

## 再開時に過去の出力を覚えておきたい場合

さて、いままでは処理の中断・再開を扱ってきましたが。出力を全て通して行いつつ、実際の処理自体は中断・再開したいということがあります。例えば、先程のスクリプトが以下のようなMakefileに書かれていたタスクだったとします。

```make
default:
	python main.py | tee output.csv
```

ここで、処理を再開したときには、過去分も含めた全ての実行結果が渡されて欲しいはずです(もちろん、呼び出すスクリプト側でファイル入出力を行い、追記でやるという案もあります)。

このようなときにちょっと一手間を加えると良い感じにできます。

```python
import time
import sys
import csv
import csvresumable


def gen(files):
    source = itertools.chain.from_iterable([csv.DictReader(open(f)) for f in files])
    return csvresumable.concat_groupby(source, key=lambda row: row["groupId"])

# captueで包んだ
with csvresumable.capture():
    for group_id, rows in csvresumable.iterate(gen(["input.csv", "input2.csv"])):
        print("start group_id", group_id, file=sys.stderr)
        time.sleep(2)
        print("total", sum(int(row["cache"]) for row in rows))
        sys.stdout.flush()  # 呼び出し方によってはbufferingされてしまう場合もある
```

captureという名前が良いかはまだ微妙ですが。このコンテキストマネージャでくるんであげるとその間の出力を記録しておけます(ちなみに引数で記録したいstreamは変更できます。デフォルトが標準出力)。
再開時には記録していた出力を再度出力してくれるため、再開(resume)時にも過去も含めた全てを出力をしてれるようになります。

```console
$ make
python main.py | tee output.txt
start group_id 1
total 1200
start group_id 2
	KeyboardInterrupt
make: *** [Makefile:2: default] Error 130

$ RESUME=1 make
python main.py | tee output.txt
total 1200
start group_id 2
total 0
start group_id 3
total 100
```

そんなわけでteeを使っていても、再開後のファイル中に全ての結果が残ります。

```console
$ cat output.txt
total 1200
total 0
total 100
```

## 最後に

裏側の話はまた今度。
