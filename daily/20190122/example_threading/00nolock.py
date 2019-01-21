# https://opensource.com/article/17/4/grok-gil

import threading
n = 0
lock = threading.Lock()


def foo():
    global n
    n += 1


threads = [threading.Thread(target=foo) for i in range(100000)]
for th in threads:
    th.start()
for th in threads:
    th.join()
print(n)
