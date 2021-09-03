from concurrent.futures import ThreadPoolExecutor


class S:
    def __init__(self):
        self.cnt = 0

    def plus(self, n):
        for i in range(n):
            self.cnt += 1


s = S()
N = 1000000
with ThreadPoolExecutor() as ex:
    for i in range(3):
        ex.submit(s.plus, N)
print(s.cnt)  # 1800308
print(3 * N)  # 3000000
