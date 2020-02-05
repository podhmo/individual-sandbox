from minitask.worker.consoleworker import Manager

with Manager().open_reader_queue("xxx.cache") as q:
    for item in q:
        print(item)
