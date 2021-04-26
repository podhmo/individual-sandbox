def run(L):
    stack = []
    indices = []
    this = L
    lv = -1
    # print(L)
    # print("")

    while True:
        if isinstance(this, list):
            stack.append(this)
            indices.append(0)
            lv += 1
            this = this[0]
        else:  # tuple
            print(this)
            # print(this, "##", lv, indices)
            indices[lv] += 1

            if len(stack[lv]) > indices[lv]:
                this = stack[lv][indices[lv]]
                continue

            print("")
            while True:
                lv -= 1
                indices[lv] += 1
                indices.pop()
                stack.pop()
                if not stack:
                    return
                if len(stack[lv]) > indices[lv]:
                    this = stack[lv][indices[lv]]
                    break


# [1, 2, 3, 4, 5]
# [0, 1, 2, 3]
# [10, 11, 12]


# [][][]int
# L = [
#     [[(i0, i1, i2) for i2 in range(1, 5 + 1)] for i1 in range(0, 3 + 1)]
#     for i0 in range(10, 12 + 1)
# ]
# L = [[[(i1, i2) for i2 in range(1, 5 + 1)] for i1 in range(0, 3 + 1)]]
L = [
    [
        [(10, 0, 1), (10, 0, 2), (10, 0, 3), (10, 0, 4), (10, 0, 5)],
        [(10, 1, 1), (10, 1, 2), (10, 1, 3), (10, 1, 4), (10, 1, 5)],
        [(10, 2, 1), (10, 2, 2), (10, 2, 3), (10, 2, 4), (10, 2, 5)],
        [(10, 3, 1), (10, 3, 2), (10, 3, 3), (10, 3, 4), (10, 3, 5)],
    ],
    [
        [(11, 0, 1), (11, 0, 2), (11, 0, 3), (11, 0, 4), (11, 0, 5)],
        [(11, 1, 1), (11, 1, 2), (11, 1, 3), (11, 1, 4), (11, 1, 5)],
        [(11, 2, 1), (11, 2, 2), (11, 2, 3), (11, 2, 4), (11, 2, 5)],
        [(11, 3, 1), (11, 3, 2), (11, 3, 3), (11, 3, 4), (11, 3, 5)],
    ],
    [
        [(12, 0, 1), (12, 0, 2), (12, 0, 3), (12, 0, 4), (12, 0, 5)],
        [(12, 1, 1), (12, 1, 2), (12, 1, 3), (12, 1, 4), (12, 1, 5)],
        [(12, 2, 1), (12, 2, 2), (12, 2, 3), (12, 2, 4), (12, 2, 5)],
        [(12, 3, 1), (12, 3, 2), (12, 3, 3), (12, 3, 4), (12, 3, 5)],
    ],
]

run(L)
