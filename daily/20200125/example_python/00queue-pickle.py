import pickle
import queue

q = queue.Queue()
q.put(1)
q.put({"name": "foo", "age": 20})
# TypeError: cannot pickle '_thread.lock' object
# print(pickle.dumps(q))

print(pickle.dumps(q.queue))
print(pickle.loads(pickle.dumps(q.queue)))  # => deque([1, {'name': 'foo', 'age': 20}])
