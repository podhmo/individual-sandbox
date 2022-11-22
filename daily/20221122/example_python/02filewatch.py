import selectors
import sys


sel = selectors.DefaultSelector()


def accept(sock, mask):
    print("@", sock, mask)


with open(sys.argv[1]) as f:
    sel.register(f, selectors.EVENT_READ, accept)

    while True:
        events = sel.select()
        for key, mask in events:
            callback = key.data
            callback(key.fileobj, mask)
