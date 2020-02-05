from minitask.worker.consoleworker import Manager

tasks = ["foo", "bar", "boo"]
m = Manager()

with m.open_writer_queue(None) as q:
    for item in tasks:
        q.put(item)
