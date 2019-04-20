for i in range(4):
    print(i)
else:
    print("else")

print("----------------------------------------")

for i in range(4):
    print(i)
    if i == 3:
        break
else:
    print("else1")

print("----------------------------------------")


for i in range(0):
    print(i)
else:
    print("else2")

for i in range(10):
    for j in range(0):
        print(j)
    else:
        continue
    print("break", i)
    break
