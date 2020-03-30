import sys

print(sys.argv)
with open(sys.argv[1]) as rf:
    print(rf)
