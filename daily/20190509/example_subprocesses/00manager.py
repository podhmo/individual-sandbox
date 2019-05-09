import typing as t
import sys
import signal
import queue
import subprocess
import threading
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed

logger = logging.getLogger(__name__)


class Process:
    def __init__(
        self,
        m: "Manager",
        *,
        uid: int,
        args: t.List[str],
        consume: t.Callable[[], None],
    ) -> None:
        self.m = m
        self.uid = uid
        self.p = subprocess.Popen(
            args, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )  # todo: stderr
        self.thread = threading.Thread(target=self._run, daemon=True)
        self.consume = consume
        self._running = False

    def receive_signal(self, signum):  # xxx: timeout
        self._running = False
        return self.p.send_signal(signum)

    def run(self):
        self._running = True
        return self.thread.start()

    def _run(self):
        while self._running:
            self.consume(self)
        # todo: lifecycle


class Manager:
    def __init__(self, *, factory=None, debug=False):
        self.pool: t.Dict[int, Process] = {}
        self.factory = factory or Process

        self.ev = threading.Event()
        self._q = queue.Queue()
        self._stopping = False
        self.debug = debug

    def consume(self, p: Process):
        # stderr
        def _consume_stderr():
            for line in p.p.stderr:
                if self._stopping and not self.debug:
                    break
                print(f"[{p.uid}] E {line}", file=sys.stderr, end="")

        threading.Thread(target=_consume_stderr, daemon=True).start()  # xxx

        for line in p.p.stdout:
            if self._stopping and not self.debug:
                break
            print(f"\x1b[9{p.uid}m[{p.uid}] {line}\x1b[0m", file=sys.stdout, end="")

    def request_stop(self, signum: int, tb=None):
        self.ev.set()
        self._q.put(signum)

    def spawn(self, args: t.List[str], *, run=True) -> Process:
        uid = len(self.pool) + 1
        p = self.factory(self, uid=uid, args=args, consume=self.consume)
        self.pool[uid] = p
        if run:
            p.run()
        return p

    def join(self):
        logger.info("start")

        # sigs = set([signal.SIGINT, signal.SIGTERM, signal.SIGSTOP, signal.SIGHUP])
        sigs = [signal.SIGINT, signal.SIGTERM]
        for sig in sigs:
            signal.signal(sig, self.request_stop)

        self.ev.wait()

        while True:
            signum = self._q.get()
            if signum not in sigs:
                logger.info(
                    "ignored: signal=%d [%s]", signum, signal.Signals(signum).name
                )
                continue  # ignored (logging?)

            logger.info("send: signal=%d [%s]", signum, signal.Signals(signum).name)
            self._stopping = True
            futs = []

            with ThreadPoolExecutor() as ex:
                for p in self.pool.values():
                    futs.append(ex.submit(p.receive_signal, signum))

            for f in as_completed(futs):
                if f.exception() is None:
                    if f.result() is not None:
                        print("R", f.result())  # xxx
                else:
                    print("E", f.exception())

            for p in self.pool.values():
                retcode = p.p.poll()
                print("PID", p.uid, "retcode", retcode)
                p.p.__exit__(None, None, None)  # xxx
            break
        logger.info("end")


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    m = Manager()
    m.spawn(["python", "./gen.py"], run=True)
    m.spawn(["python", "./gen.py"], run=True)
    m.spawn(["python", "./gen.py"], run=True)
    m.spawn(["python", "./gen.py"], run=True)
    m.spawn(["python", "./gen.py"], run=True)
    m.spawn(["python", "./gen.py"], run=True)
    m.join()
