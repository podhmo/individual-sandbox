import time
import signal
import sys
import traceback
import json
import logging

logger = logging.getLogger(__name__)


def setup_graceful_stop(fn, sigs=[signal.SIGINT, signal.SIGTERM, signal.SIGQUIT, signal.SIGHUP]):
    def callback(signum, frame):
        # ここでframeが渡されるようにするとtracebackなどが取れる
        tb = traceback.format_stack(frame, limit=None)
        logger.info("tb=%s", json.dumps(tb, ensure_ascii=False))
        fn()  # 引数渡したほうが嬉しい？
        sys.exit(1)

    for s in sigs:
        signal.signal(s, callback)
    return fn


def loop():
    i = 0
    while True:
        print(i)
        time.sleep(0.5)
        i += 1


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)

    @setup_graceful_stop
    def callback(*args, **kwargs):
        # 途中でcallbackに渡す引数が変更された場合にエラーに成ってほしくないので*args,**kwargs付けておくのが無難
        print("end")

    loop()
