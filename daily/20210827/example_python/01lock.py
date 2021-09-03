from concurrent.futures import ThreadPoolExecutor
from threading import Lock


class S:
    def __init__(self):
        self.cnt = 0
        self._lock = Lock()

    def plus(self, n):
        for i in range(n):
            with self._lock:
                self.cnt += 1


s = S()
N = 1000000
with ThreadPoolExecutor() as ex:
    for i in range(3):
        ex.submit(s.plus, N)
print(s.cnt)  # 3000000
print(3 * N)  # 3000000
