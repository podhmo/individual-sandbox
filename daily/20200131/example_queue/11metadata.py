import typing as t
import random
import math
import queue

q = queue.PriorityQueue()
q.put((math.inf, None))  # sentinel
for i in range(5):
    item = (random.random(), i)
    q.put(item)

while True:
    item = q.get()
    if item[1] is None:
        q.task_done()
        break
    print(item)
    q.task_done()
print("ok")


def create_message(item, *, priority: float, **metadata: t.Any):
    return (priority, item)


def extract_body(message):
    return message[1]


q = queue.PriorityQueue()
q.put((math.inf, None))  # sentinel
for i in range(5):
    m = create_message(i, priority=random.random())
    q.put(m)

while True:
    m = q.get()
    item = extract_body(m)
    if item is None:
        q.task_done()
        break
    print(item)
    q.task_done()
print("ok")
