import threading
import time

counter = 5
L = threading.Lock()


def do_someting(name, sleep_time):
    global counter
    count = counter
    print(f'{name}[R]	counter:{counter}')
    time.sleep(sleep_time)

    counter = count - 1
    print(f'{name}[W]	counter:{counter}')


thread1 = threading.Thread(target=do_someting, args=('a', 1))
thread2 = threading.Thread(target=do_someting, args=('b', 0.1))
thread1.start()
time.sleep(0.1)
thread2.start()
thread1.join()
thread2.join()
print('counter: ' + str(counter))
