def calc(x,n,*,d):
    y_x = x * 12
    return (d,sum([y_x for _ in range(n)]), sum([y_x * (d ** i) for i in range(n)]))

# 3万   30年    5%
for i in range(0, 11):
    d = 1 + 0.01 * i
    print(calc(3,30,d=d))
