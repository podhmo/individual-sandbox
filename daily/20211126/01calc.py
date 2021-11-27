import nikkei
import math


def calc(start_year:int, x:int,n:int):
    x = x * 10000
    base_list = nikkei.get_base(start_year, n)
    # base_list = [10000 * (1.03 ** i) for i in range(n)]
    base_list = [10000 + (5000 * math.sin(math.radians(i))) for i in range(40*(start_year-1980), 40 * (start_year-1980+n), 40)]
    r = [x*12/base_list[0]]
    for prev_base, base in zip(base_list, base_list[1:]):
        r.append(r[-1] * (base/prev_base) + x/base * 12)

    cost = sum([x * 12 for _ in range(len(r))])

    last_base = base_list[-1]
    # last_base = nikkei.get_base(2021,1)[0]
    benefit = r[-1]*last_base
    return (start_year, x, len(r)), cost/10000, benefit/10000, benefit/cost

for year in range(1980, 2022, 1):
    print(calc(year, 3, 30))