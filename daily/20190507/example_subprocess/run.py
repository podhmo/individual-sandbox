import itertools
import typing as t
import subprocess
import queue
import signal
import threading

q = queue.Queue()
workers: t.List[threading.Thread] = []
processes: t.List[subprocess.Popen] = []
i = 0


def cands():
    cands = itertools.product(
        [0, 1, 4, 7],
        [30, 31, 32, 33, 34, 35, 36, 37, 90, 91, 92, 93, 94, 95, 96, 97],
        [40, 41, 42, 43, 44, 45, 46, 47],
    )
    # (None, bold, underscore, reversed), Foregroupd, Background
    return [
        (j, k) if i == 0 else (i, j, k)
        for (i, j, k) in cands
        if str(j)[-1] != str(k)[-1]
    ]


symbols = [";".join(map(str, xs)) for xs in cands()]


def spawn(uid: str, cmd: t.List[str]) -> None:
    global i
    p = subprocess.Popen(cmd, text=True, stdout=subprocess.PIPE)
    c = symbols[i % len(symbols)]
    i += 1

    def run(p=p):
        with p:
            for line in p.stdout:
                q.put((c, uid, line))

    processes.append(p)
    th = threading.Thread(target=run, daemon=True)
    workers.append(th)
    th.start()


[spawn(str(i), ["python", "gen.py"]) for i in range(50)]


def handler(signum, tb):
    q.put((None, None, None))


signal.signal(signal.SIGINT, handler)
while True:
    c, uid, line = q.get()
    if line is None:
        break
    print(f"\x1b[{c}m{uid.rjust(2, ' ')}: {line.rstrip().ljust(40, ' ')}\x1b[0m")

for p in processes:
    p.send_signal(signal.SIGINT)
