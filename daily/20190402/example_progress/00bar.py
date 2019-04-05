import time

for i in range(10):
    print(f'\r\033[K{"*" * i}', end="")
    time.sleep(0.5)
print("ok")
