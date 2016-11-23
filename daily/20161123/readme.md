# arch linux

- https://www.archlinux.org/download/
- https://wiki.archlinux.org/index.php/Installation_guide


# apt sources.list change (en -> jp)

```
sudo sed -i.bak -e "s%http://us.archive.ubuntu.com/ubuntu/%http://ftp.iij.ad.jp/pub/linux/ubuntu/archive/%g" /etc/apt/sources.list
```

see: http://qiita.com/fkshom/items/53de3a9b9278cd524099


# golangで特定の条件を達した時に `go install` や `go build` がruntimeエラーを起こす

https://gist.github.com/podhmo/67605e2119a50dcf61bfd5e9f4e12206

あとで調べたい。

# golang GOPATHとGOROOTを見つける方法

shell上から

```
$ go env | grep -P 'GOPATH|GOROOT'
GOPATH="/home/<me>/go"
GOROOT="/opt/local/lib/go"
```

goのコード上

```
$ gore
gore version 0.2.6  :help for help
gore> :import os
gore> os.Getenv("GOPATH")
"/Users/me/go"
gore> :import runtime
gore> runtime.GOROOT()
"/opt/local/lib/go"
```

# Makefile こういうmakefile好き

make generateしたときには.tmpに一時ファイルが出力。通常はテキトウな一時ディレクトリに一時ファイルが出力。


```make
TMPDIR ?= .tmp

default:
	TMPDIR=`mktemp -d` make generate; unset TMPDIR

generate:
	mkdir -p ${TMPDIR}
	echo hai > ${TMPDIR}/hai.txt
	echo hello > ${TMPDIR}/hello.txt
	echo bye > ${TMPDIR}/bye.txt
	tree ${TMPDIR}
	# cleanup if needed

.PHONY: generate
```

使うときはこういう感じ。

```bash
$ make
TMPDIR=`mktemp -d` make generate; unset TMPDIR
echo hai > /var/folders/yc/_jqyszhd7zsdzym4cmtjl0r9sj_6zv/T/tmp.NOIUKcvv/hai.txt
echo hello > /var/folders/yc/_jqyszhd7zsdzym4cmtjl0r9sj_6zv/T/tmp.NOIUKcvv/hello.txt
echo bye > /var/folders/yc/_jqyszhd7zsdzym4cmtjl0r9sj_6zv/T/tmp.NOIUKcvv/bye.txt
tree /var/folders/yc/_jqyszhd7zsdzym4cmtjl0r9sj_6zv/T/tmp.NOIUKcvv
/var/folders/yc/_jqyszhd7zsdzym4cmtjl0r9sj_6zv/T/tmp.NOIUKcvv
├── bye.txt
├── hai.txt
└── hello.txt

0 directories, 3 files
# cleanup if needed
```
