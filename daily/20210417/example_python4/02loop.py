# L = [
#     [[(x, y, z) for z in range(100, 500, 100)] for y in range(10, 40, 10)]
#     for x in range(3)
# ]
# print(L)
L = [
    [
        [(0, 10, 100), (0, 10, 200), (0, 10, 300), (0, 10, 400)],
        [(0, 20, 100), (0, 20, 200), (0, 20, 300), (0, 20, 400)],
        [(0, 30, 100), (0, 30, 200), (0, 30, 300), (0, 30, 400)],
    ],
    [
        [(1, 10, 100), (1, 10, 200), (1, 10, 300), (1, 10, 400)],
        [(1, 20, 100), (1, 20, 200), (1, 20, 300), (1, 20, 400)],
        [(1, 30, 100), (1, 30, 200), (1, 30, 300), (1, 30, 400)],
    ],
    [
        [(2, 10, 100), (2, 10, 200), (2, 10, 300), (2, 10, 400)],
        [(2, 20, 100), (2, 20, 200), (2, 20, 300), (2, 20, 400)],
        [(2, 30, 100), (2, 30, 200), (2, 30, 300), (2, 30, 400)],
    ],
]
this = L

stack = []
pc = []
idx = -1

stack.append(this)
pc.append(0)
idx += 1
this = this[0]

stack.append(this)
pc.append(0)
idx += 1
this = this[0]

stack.append(this)
pc.append(0)
idx += 1
this = this[0]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
assert len(stack[idx]) == pc[idx]
idx -= 1
pc[idx] += 1
for k in range(idx + 1, len(pc)):
    pc[k] = 0
pc.pop()
stack.pop()
this = stack[idx][pc[idx]]
print("")
print("----------------------------------------")
print("")

stack.append(this)
pc.append(0)
idx += 1
this = this[0]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
assert len(stack[idx]) == pc[idx]
idx -= 1
pc[idx] += 1
for k in range(idx + 1, len(pc)):
    pc[k] = 0
pc.pop()
stack.pop()
this = stack[idx][pc[idx]]
print("")
print("----------------------------------------")
print("")

stack.append(this)
pc.append(0)
idx += 1
this = this[0]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
this = stack[idx][pc[idx]]

print(this, "##", idx, pc)
pc[idx] += 1
assert len(stack[idx]) == pc[idx]
idx -= 1
pc[idx] += 1
for k in range(idx + 1, len(pc)):
    pc[k] = 0
pc.pop()
stack.pop()
assert len(stack[idx]) == pc[idx]

# idx -= 1
# pc.pop()
# stack.pop()
# pc[idx] += 1
# this = stack[idx][pc[idx]]
# print("")
# print("----------------------------------------")
# print("")

print(this, "##", idx, pc)
stack.append(this)
pc.append(0)
idx += 1
this = this[0]

print(this, "##", idx, pc)
# [
#     [
#         [
#             [(0, 10, 100), (0, 10, 200), (0, 10, 300), (0, 10, 400)],
#             [(0, 20, 100), (0, 20, 200), (0, 20, 300), (0, 20, 400)],
#             [(0, 30, 100), (0, 30, 200), (0, 30, 300), (0, 30, 400)],
#         ],
#         [
#             [(1, 10, 100), (1, 10, 200), (1, 10, 300), (1, 10, 400)],
#             [(1, 20, 100), (1, 20, 200), (1, 20, 300), (1, 20, 400)],
#             [(1, 30, 100), (1, 30, 200), (1, 30, 300), (1, 30, 400)],
#         ],
#         [
#             [(2, 10, 100), (2, 10, 200), (2, 10, 300), (2, 10, 400)],
#             [(2, 20, 100), (2, 20, 200), (2, 20, 300), (2, 20, 400)],
#             [(2, 30, 100), (2, 30, 200), (2, 30, 300), (2, 30, 400)],
#         ],
#     ]
# ]
# [
#     [(0, 10, 100), (0, 10, 200), (0, 10, 300), (0, 10, 400)],
#     [(0, 20, 100), (0, 20, 200), (0, 20, 300), (0, 20, 400)],
#     [(0, 30, 100), (0, 30, 200), (0, 30, 300), (0, 30, 400)],
# ]

