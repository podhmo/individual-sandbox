import queue


q = queue.Queue()
for i in range(5):
    q.put(i)
q.put(None)

q = CustomQueue(q)
while True:
    # 見た目をなるべくqueue.Queueに揃えたこの形で考えてみる
    # 例えばSQSを扱ったときにはtask_done()を読んだときにdelete_message()が呼ばれてほしい
    # get()時にclosure()も一緒に返している
    # どのmessageを消すかの所属の情報がほしいので、単にq.task_done()を呼ぶ形ではうまくいかない
    # 必要なものがidだけに限らない可能性がある(e.g. ReceiptHandle も渡したい)
    # そしてこの方針だとうまくdelete_message_batch()を呼ぶことはできない

    item, task_done = q.get()
    if item is None:
        task_done()
        break
    # do something
    task_done()
