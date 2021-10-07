import sys

# https://discord.com/channels/410969055495847936/594767462608797697/894926529639907368


def solve(itr):
    ans = ([],[])
    for n in itr:
        ans[n%2].append(n)
        yield ans
    return ans


def ask(prompt):
    while True:
        try:
            n = int(input(prompt))
            yield n
        except ValueError:
            break

# debug:
# print(list(solve([1,2,3,4,5,6]))[-1])

for ans in solve(ask("Enter a number: ")):
    print("\t", ans,file=sys.stderr)
print("answer is ", ans)
