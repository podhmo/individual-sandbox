# python asyncio

他の並行制御(selector, AsyncQueue)

asyncqueue

```python
async def do_loop():
    aq = AsyncQueue(2)
    nums = list(range(5))
    while nums or not aq.empty():
        if nums and not aq.full():
            uid = nums.pop()
            await aq.put(do_task(uid))
        else:
            uid = await aq.get()
            logger.info("gotcha %s", uid)
```

selector

```python
async def do_loop2():
    aq = AsyncQueue(2)
    selector = Selector(aq)
    for uid in range(5):
        logger.info("insert %s", uid)
        result = await selector.put_untill_success(do_task, uid)
        logger.info("gotcha %s", result)
    while not aq.empty():
        logger.info("gotcha %s", await aq.get())
```

# python PyYAML でscannerの受理するsymbolを変更する

ちょっと難しそう。このあたりのmethodがあるので `"@foo: key"` とか無理みたい。
無難なのは `"$foo: key"` とかか。

# python pythonのyaml loadでjsonと互換性がある感じに読み込む

keyがintをjsonは許さない。

```
Python 3.5.2 (default, Sep 19 2016, 02:49:52)
[GCC 4.2.1 Compatible Apple LLVM 7.3.0 (clang-703.0.31)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> import json
>>> json.dumps({100: 200})
'{"100": 200}'
```

```python
# scanner.py
    def check_plain(self):

        # A plain scalar may start with any non-space character except:
        #   '-', '?', ':', ',', '[', ']', '{', '}',
        #   '#', '&', '*', '!', '|', '>', '\'', '\"',
        #   '%', '@', '`'.
        #
        # It may also start with
        #   '-', '?', ':'
        # if it is followed by a non-space character.
        #
        # Note that we limit the last rule to the block context (except the
        # '-' character) because we want the flow context to be space
        # independent.
        ch = self.peek()
        return ch not in '\0 \t\r\n\x85\u2028\u2029-?:,[]{}#&*!|>\'\"%@`'  \
                or (self.peek(1) not in '\0 \t\r\n\x85\u2028\u2029'
                        and (ch == '-' or (not self.flow_level and ch in '?:')))
```

# python logging 常に特定の引数を付加したい

例えば asctime(現在時刻) の代わりに経過時刻にしたい。

log record factoryを変更するのが楽そう

```python
class Extension:
    def __init__(self):
        self.c = defaultdict(int)


class LogRecordExtension(logging.LogRecord):
    extension = Extension()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.extension.c[self.msg] += 1
        self.call_count = self.extension.c[self.msg]


logging.setLogRecordFactory(LogRecordExtension)
```
