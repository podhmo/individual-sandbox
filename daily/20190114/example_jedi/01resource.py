import sys
import threading
queue = []


def _enqueue_output(out):
    for line in iter(out.readline, b''):
        queue.append(line)
    out.close()


t = threading.Thread(target=_enqueue_output, args=(sys.stdin, ), daemon=True)
t.start()
print("hoi")
sys.stdin.close()
print(queue)
