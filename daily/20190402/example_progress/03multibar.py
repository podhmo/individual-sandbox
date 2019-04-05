import time


print("\0337", end="")
for i in range(10):
    print("\0338\033[J", end="")
    for j in range(3):
        print(f'{"*" * i}')
    time.sleep(0.5)
print("ok")
