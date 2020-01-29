import queue

q = queue.Queue()

for i in range(5):
    q.put(i)
q.put(None)

while True:
    item = q.get()
    if item is None:
        break
    print(item)
