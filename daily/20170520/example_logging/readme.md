loggingってキーワード引数的なものついかできたっけ？

## hmm

と言うか以下のページを熟読すると良さそう。

- [Logging クックブック — Python 3.6.1 ドキュメント](https://docs.python.jp/3/howto/logging-cookbook.html)

用語集

- Logger
- Handler
- Formatter
- Filter
- LoggerAdapter
- LogRecord(LogRecordFactory)

## 探索

たしかLogRecordというのがメッセージを作っていたのだよなー。

```python
class LogRecord(object):
    def getMessage(self):
        """
        Return the message for this LogRecord.

        Return the message for this LogRecord after merging any user-supplied
        arguments with the message.
        """
        msg = str(self.msg)
        if self.args:
            msg = msg % self.args
        return msg
```

ただ、このgetMessageは実際どこで使っているかというと。Formatterのformat()

```python
class Formatter(object):
    def format(self, record):
        """
        Format the specified record as text.

        The record's attribute dictionary is used as the operand to a
        string formatting operation which yields the returned string.
        Before formatting the dictionary, a couple of preparatory steps
        are carried out. The message attribute of the record is computed
        using LogRecord.getMessage(). If the formatting string uses the
        time (as determined by a call to usesTime(), formatTime() is
        called to format the event time. If there is exception information,
        it is formatted using formatException() and appended to the message.
        """
        record.message = record.getMessage()
        if self.usesTime():
            record.asctime = self.formatTime(record, self.datefmt)
        s = self.formatMessage(record)
        if record.exc_info:
            # Cache the traceback text to avoid converting it multiple times
            # (it's constant anyway)
            if not record.exc_text:
                record.exc_text = self.formatException(record.exc_info)
        if record.exc_text:
            if s[-1:] != "\n":
                s = s + "\n"
            s = s + record.exc_text
        if record.stack_info:
            if s[-1:] != "\n":
                s = s + "\n"
            s = s + self.formatStack(record.stack_info)
        return s
```

あと何か弄るときにはLoggerAdapterがあり。これを使うと便利。
