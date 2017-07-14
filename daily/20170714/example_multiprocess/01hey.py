from multiprocessing import *


def child(task_q, result_q):
    while True:
        print("  Getting task...")
        task = task_q.get()
        print("  Got task", task[:10])
        task = task * 100000000
        print("  Putting result", task[:10])
        result_q.put(task)
        print("  Done putting result", task[:10])
        task_q.task_done()


def parent():
    task_q = JoinableQueue()
    result_q = JoinableQueue()
    worker = Process(target=child, args=(task_q, result_q))
    worker.daemon = True
    worker.start()
    #tasks = ["foo", "bar", "ABC" * 100000000, "baz"]
    tasks = ["foo", "bar", "ABC", "baz"]
    for task in tasks:
        print("Putting task", task[:10], "...")
        task_q.put(task)
        print("Done putting task", task[:10])
    task_q.join()
    for task in tasks:
        print("Getting result...")
        print("Got result", result_q.get()[:10])


if __name__ == '__main__':
    parent()
