import timeit


print(timeit.repeat("os.environ.get('A')", setup="import os"))
print(timeit.repeat("d.get('A')", setup="import os; d = {}"))
print(timeit.repeat("d.get('A')", setup="import os; d = {'A': 'a'}"))

# [1.7474516200018115, 1.760859269008506, 1.756943639004021]
# [0.1132325380021939, 0.11371001599763986, 0.11390991799999028]
# [0.11703252598817926, 0.11999797499447595, 0.11053714400622994]
