import dataclasses


@dataclasses.dataclass
class Loop:
    i: int
    z: int
    n: int


stack = []
stack.append(Loop(i=10, z=10, n=12))
stack.append(Loop(i=0, z=0, n=3))
stack.append(Loop(i=1, z=1, n=5))


N = len(stack)
idx = N - 1

print(stack)
print("----------------------------------------")
while True:
    # print(idx, stack)
    print(idx, [f.i for f in stack])
    f = stack[idx]
    if f.i < f.n:
        f.i += 1
        continue

    if idx <= 0:
        break
    idx -= 1

    stack[idx].i += 1
    if stack[idx].i <= stack[idx].n:
        for k in range(idx + 1, N):
            stack[k].i = stack[k].z
        idx = N - 1
