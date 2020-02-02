import queue


q = queue.Queue()
for i in range(5):
    q.put(i)
q.put(None)

q = CustomQueue2(q)
while True:
    # task_doneがitemを取れるようにしてみる。
    # 一度に複数のmessageを受け取る場合もありその場合はitemがlistになっている
    # エラーで失敗した場合にはdelete_message()が呼ばれてほしくないということを満たした形
    # (一方でconnection less的に使った場合には、単にlogだけ吐いて終了してほしい)
    item = q.get()
    if item is None:
        q.task_done(item)
        break
    # do something
    q.task_done(item)
