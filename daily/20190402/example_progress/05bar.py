import time


n = 3

for i in range(11):
    if i > 0:
        print(f"\033[{n}F\033[J", end="")
    for j in range(n):
        print(f"{i * 10: 2}% |{'████' * i}")
    time.sleep(0.5)
print("ok")
