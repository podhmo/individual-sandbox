## python named pipeで良いのかもしれない

- https://www.eadan.net/blog/ipc-with-named-pipes/
- http://cheesehead-techblog.blogspot.com/2013/12/handy-pipe-and-socket-reference.html

### os.openつらい

通常だと `O_CREAT` が付いてしまう。。
openにopenerを渡してあげればよいのでは？ 

- https://github.com/python/cpython/blob/master/Lib/_pyio.py

flagsをhijack

```python
    # NOT O_CREAT
    def _opener(path: str, flags: int) -> int:
        return os.open(path, os.O_WRONLY)

    with open(endpoint, "w", opener=_opener) as wf:
```
