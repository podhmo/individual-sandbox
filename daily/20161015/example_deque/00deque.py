from collections import deque

dq = deque(maxlen=3)

for i in range(6):
    dq.append(i)
    print(dq)
