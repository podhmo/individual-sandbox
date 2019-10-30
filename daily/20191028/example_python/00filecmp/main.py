import filecmp


def run(left, right):
    print(left, right, filecmp.cmp(left, right))


run("hello.txt", "hello.txt")
run("hell0.txt", "hello.txt")
run("hello.txt", "bye.txt")
