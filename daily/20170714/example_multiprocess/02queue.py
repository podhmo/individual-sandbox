from multiprocessing.queues import Queue, context
q = Queue(2, ctx=context._default_context)
q.put_nowait(None)
q.put_nowait(None)
q.put_nowait(None)
