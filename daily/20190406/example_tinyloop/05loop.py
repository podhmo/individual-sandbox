from concurrent import futures as base_futures

CancelledError = base_futures.CancelledError
InvalidStateError = base_futures.InvalidStateError
_PENDING = base_futures._PENDING
_CANCELLED = base_futures._CANCELLED
_FINISHED = base_futures._FINISHED


class Future:
    def __init__(self, loop):
        self.loop = loop
        self._state = _PENDING
        self._result = None
        self._exception = None
        self._callbacks = []

    def done(self):
        return self._state != _PENDING

    def result(self):
        if self._state == _CANCELLED:
            raise CancelledError
        if self._state != _FINISHED:
            raise InvalidStateError("Result is not ready.")
        if self._exception is not None:
            raise self._exception
        return self._result

    def set_result(self, result):
        if self._state != _PENDING:
            raise InvalidStateError("{}: {!r}".format(self._state, self))
        self._result = result
        self._state = _FINISHED

        callbacks = self._callbacks[:]
        if not callbacks:
            return
        self._callbacks[:] = []
        for cb in callbacks:
            self._loop.call_soon(cb, self)

    def add_done_callback(self, fn):
        if self._state != _PENDING:
            self._loop.call_soon(fn, self)
        else:
            self._callbacks.append(fn)


def is_future(x):
    return isinstance(x, Future)


class Loop:
    def __init__(self):
        self._stoping = False
        self._ready = []

    def stop(self):
        self._stoping = True

    def create_future(self):
        return Future(loop=self)

    def call_soon(self, fut):
        pass

    def run_until_complete(self, fut):
        # - ensure_future(fut)
        # - add_done_callback()にcloseするコードを入れる
        # run_forever()
        # futが終わらない内に止まった場合はエラー
        pass

    def run_forever(self):
        while True:
            self._run_once()
            if self._stopping:
                break
