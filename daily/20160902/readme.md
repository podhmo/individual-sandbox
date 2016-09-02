# golang deferで関数の戻り値を変える

named return使うしか無い

# python 簡単なthread

loggingを追加したい場合には、threading.Threadのsubclass作ってやったほうが良い。

```python
class TBThread(threading.Thread):
    def run(self, *args, **kwargs):
        try:
            return super().run(*args, **kwargs)
        except Exception as e:
            logger.warn(e, exc_info=True)
            raise
```

