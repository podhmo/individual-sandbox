import itertools
for i in range(40, 47+1):
    print(f"\x1b[{i}m   {i}   \x1b[0m", end="")
print("")

for i in itertools.chain(range(30, 37+1), range(90, 97+1)):
    for j in range(40, 47+1):
        print(f"\x1b[{i};{j}m {i};{j}  \x1b[0m", end="")
    print("")
    for k in [1, 4, 7]:
        for j in range(40, 47+1):
            print(f"\x1b[{i};{j};{k}m {i};{j};{k}\x1b[0m", end="")
        print("")
