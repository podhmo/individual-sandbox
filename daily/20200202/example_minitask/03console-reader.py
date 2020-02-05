from minitask.worker.consoleworker import Manager

m = Manager()

with m.open_reader_queue(None) as q:
    for item in q:
        print("got", item)
