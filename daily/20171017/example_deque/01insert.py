from collections import deque

dq = deque([1, 2, 3], maxlen=10)
dq.insert(1, 10)
dq.append(20)
dq.appendleft(30)
print(list(dq))  # [30, 1, 10, 2, 3, 20]

L = [1, 2, 3]
L.insert(1, 10)
L.append(20)
L.insert(0, 30)
print(L)  # [30, 1, 10, 2, 3, 20]
