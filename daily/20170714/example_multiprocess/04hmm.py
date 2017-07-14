import logging
import threading
import weakref
from concurrent.futures import ProcessPoolExecutor, as_completed, _queue_management_worker, _threads_queues


def get(i):
    import os
    print(os.getpid())
    import time
    time.sleep(10)
    return i * i


def wrap(*args, **kwargs):
    return _queue_management_worker(*args, **kwargs)


class E(ProcessPoolExecutor):
    def _start_queue_management_thread(self):
        # When the executor gets lost, the weakref callback will wake up
        # the queue management thread.
        def weakref_cb(_, q=self._result_queue):
            q.put(None)

        if self._queue_management_thread is None:
            # Start the processes so that their sentinels are known.
            self._adjust_process_count()
            self._queue_management_thread = threading.Thread(
                target=wrap,
                args=(
                    weakref.ref(self, weakref_cb), self._processes, self._pending_work_items,
                    self._work_ids, self._call_queue, self._result_queue
                )
            )
            self._queue_management_thread.daemon = True
            self._queue_management_thread.start()
            _threads_queues[self._queue_management_thread] = self._result_queue


try:
    with E(10) as e:
        futs = []
        for i in range(20):
            print("add", i)
            f = e.submit(get, i)
            futs.append(f)

        for f in as_completed(futs):
            try:
                print("result", f.result())
            except Exception as e:
                logging.debug(str(e), exc_info=True)
except Exception as e:
    print("@", e)

logging.basicConfig(level=logging.DEBUG)
