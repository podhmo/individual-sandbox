# pip install jqfpy

SRC ?= data/sample0.json

## 1.読みやすいよう整形する
## 長いJSONファイルの場合だと長くなるので、そういった時は「-c」をつけると良いだろう。
00:
	jqfpy -c ${SRC}

## 2.値のみを抽出する
## jqコマンドでは、値を抽出したいキーを指定することで、その値のみ列挙させる事ができる。
01:
	jqfpy '[d["id"] for d in get()]' ${SRC}  --squash
	jqfpy '[d["group1"]["subg01"] for d in get()]' ${SRC}  --squash -c
	jqfpy 'get("[]/group1/subg01[]/id")' ${SRC}  --squash --squash -c
	# jqfpy 'from dictknife import DictWalker; [d["id"] for _, d in DictWalker(["id"]).walk(get())]' ${SRC}

## 3.条件に合わせて抽出を行う
## ‘select(キー名 条件 値)’といった指定を行う事で、条件に合致した配列やキーを取得することができる。
## なお、検索対象が配列の場合、select前に配列の殻割りをしないとトップレベルで出てきてしまうので注意。
02:
	jqfpy 'xs = [[x for x in get("group1/subg01", d=d) if x["id"] == "1021"] for d in get()]; [x for x in xs if x]' ${SRC} --squash -c --squash
	jqfpy '[d for d in get() if "1021" in get("group1/subg01[]/id", d=d)]' ${SRC} --squash

## 4.「and」「or」「not」で指定する
## jqコマンドでは、「and」「or」「not」での条件指定にも対応している。
## select内で条件指定することで対象の配列の情報を抽出できる(そのまま使うと、True/Falseが返ってくる)。

03:
	jqfpy '' ${SRC}
