import time


for i in range(10):
    if i > 0:
        print("\033[3F\033J", end="")
    for j in range(3):
        print(f'{"*" * i}')
    time.sleep(0.5)
print("ok")
