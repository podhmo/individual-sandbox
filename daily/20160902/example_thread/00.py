import threading
import logging
import time


logger = logging.getLogger(__name__)
_is_fake = False


def set_fake_async_task(status):
    global _is_fake
    _is_fake = status


def get_fake_async_task():
    global _is_fake
    return _is_fake


class TBThread(threading.Thread):
    def run(self, *args, **kwargs):
        try:
            return super().run(*args, **kwargs)
        except Exception as e:
            logger.warn(e, exc_info=True)
            raise


def run_with_thread(method, *args, **kwargs):
    if get_fake_async_task():
        method(*args, **kwargs)
        return
    thread = TBThread(target=lambda: method(*args, **kwargs))
    thread.start()
    logger.debug(
        'started thread: id=%s, method=%s, args=%s, kwargs=%s',
        thread.ident,
        method,
        args,
        kwargs)
    return thread


def main():
    logging.basicConfig(level=logging.DEBUG, filename="00.log.txt")

    def f():
        print("hai..")
        time.sleep(0.5)
        print("..hai")

    def g():
        raise Exception("oops")

    run_with_thread(f)
    run_with_thread(g)
    time.sleep(1)
    print("ok")

if __name__ == "__main__":
    main()
