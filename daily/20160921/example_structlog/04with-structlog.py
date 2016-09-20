"""
structlogとloggingを上手く組み合わせたい -> 無理
wip
"""
import logging
import sys
logger = logging.getLogger(__name__)
logging.basicConfig(level=logging.DEBUG)


class MyHandler(logging.StreamHandler):
    def emit(self, record):
        try:
            print(record.__dict__)  # extraが何だったのかわからなくなる点でダメ。
            # print(record.age)
            msg = self.format(record)
            stream = self.stream
            stream.write(msg)
            stream.write(self.terminator)
            self.flush()
        except Exception:
            self.handleError(record)


class LTSVFormatter(logging.Formatter):
    def format(self, record):
        from collections import OrderedDict
        data = OrderedDict()
        data['pid'] = record.process
        data['level'] = record.levelname

        # ログメッセージが辞書の場合には出力データにそのままマッピングする
        if isinstance(record.msg, str):
            data['msg'] = record.msg
        elif isinstance(record.msg, dict):
            data.update(record.msg)

        return ltsv.writer(data).decode('utf-8')


class MyFormatter(logging.Formatter):
    def format(self, record):
        # ここを書き換える。適切に。。
        return super().format(record)


def makeRecord(self, name, level, fn, lno, msg, args, exc_info,
               func=None, extra=None, sinfo=None):
    """
    A factory method which can be overridden in subclasses to create
    specialized LogRecords.
    """
    rv = logging._logRecordFactory(name, level, fn, lno, msg, args, exc_info, func,
                                   sinfo)
    if extra is not None:
        for key in extra:
            if (key in ["message", "asctime"]) or (key in rv.__dict__):
                raise KeyError("Attempt to overwrite %r in LogRecord" % key)
            rv.__dict__[key] = extra[key]
    # これで無理矢理keyを知る
    rv.__dict__["extra"] = list(extra.keys()) if extra else []
    return rv

logging.Logger.makeRecord = makeRecord
logger.info("hello: %s", "foo", extra={"age": 21})
handler = MyHandler(sys.stdout)
handler.setFormatter(logging.Formatter(">> %(message)s"))
logger.addHandler(handler)
logger.info("hello: %s", "foo", extra={"age": 21})
