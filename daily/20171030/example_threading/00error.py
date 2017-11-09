import threading


def f():
    print("before")
    1 / 0
    print("after")


t = threading.Thread(target=f).start()
print("hoi")
t.join()
print("hoi@")
