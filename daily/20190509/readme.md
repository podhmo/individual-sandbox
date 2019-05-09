## python subprocess

- 簡単なprocess managerを作りたい(inspired by too)
- threading? [./example_subprocesses](./example_subprocesses)
- asyncioをつかっても [../20190120/example_evry](../20190120/example_evry)

  - communicateを使うのは正しくなさそう

### asyncio?

- https://docs.python.org/ja/3.7/library/asyncio-protocol.html#loop-subprocess-exec-and-subprocessprotocol
-https://docs.python.org/ja/3.7/library/asyncio-subprocess.html#asyncio-example-create-subprocess-exec
- https://docs.python.org/ja/3.7/library/asyncio-api-index.html

hmm

- https://pymotw.com/3/asyncio/subprocesses.html
- https://kevinmccarthy.org/2016/07/25/streaming-subprocess-stdin-and-stdout-with-asyncio-in-python/

## python threading

- event,barrierは悪くないのだけれど。動的に数が変わる場合に困るかも？
- queue.Queue?

## elisp lisp-2の話

- symbol-value, symbol-functionを見れば良さそう
- fsetも見れば良い？
