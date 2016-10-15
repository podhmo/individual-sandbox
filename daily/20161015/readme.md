# go sliceのcastは無理という話。

以下が無理。

```go
func doPrint(xs []interface{}) {
	for _, x := range xs {
		log.Printf("%[1]T: %#[1]v", x)
	}
}

func main(){
	xs := []int{1, 2, 3, 4, 5}
	// compile error. expected []interface{} but []int
	doPrint(xs)
}
```

こう書くのが無難そう。

```go
func main() {
	xs := []int{1, 2, 3, 4, 5}
	ys := make([]interface{}, len(xs))
	for i, x := range xs {
		ys[i] = x
	}
	doPrint(ys)
}
```

# python asyncio + rpc + shell的なinterfaceみたいなことをしてみたくなった

crawlerの管理にshellみたいなものがあると良いのではというような気持ち。

zeromqは最終的にサポートするとしてそれ以外を先にやりたいみたいな感じもある。

- [aio-libs/aiozmq](https://github.com/aio-libs/aiozmq)

# python collections.deque が便利

```python

from collections import deque

dq = deque(maxlen=3)

for i in range(6):
    dq.append(i)
    print(dq)
```

```
deque([0], maxlen=3)
deque([0, 1], maxlen=3)
deque([0, 1, 2], maxlen=3)
deque([1, 2, 3], maxlen=3)
deque([2, 3, 4], maxlen=3)
deque([3, 4, 5], maxlen=3)
```

# shell du またディスク容量が

[過去に書いていた](../20160924/readme.md)

```
sudo du -h / > /tmp/mem.txt && cat /tmp/mem.txt | grep -P '^ *([\d\.]+G|\d{3,}M)' | gsort -h
```

# git gitのrepository内の巨大なファイルを探す

現在のrevisionでサイズの大きなもの順に10件表示

```
$ git ls-tree -l -t -r --full-name HEAD | sort -n -k 4 -r | head -n 10
```

参考

- [How to find the N largest files in a git repository? - Stack Overflow](http://stackoverflow.com/questions/9456550/how-to-find-the-n-largest-files-in-a-git-repository)
- [filesize - Git: show total file size difference between two commits? - Stack Overflow](
http://stackoverflow.com/questions/10845051/git-show-total-file-size-difference-between-two-commits/10847242#10847242)
