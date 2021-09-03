from concurrent.futures import ThreadPoolExecutor


class S:
    def __init__(self):
        self.L = []

    def plus(self, n):
        for i in range(n):
            self.L.append(i)


s = S()
N = 1000000
with ThreadPoolExecutor() as ex:
    for i in range(3):
        ex.submit(s.plus, N)
print(len(s.L))  # 3000000
print(3 * N)  # 3000000
