from kamidana import as_filter

@as_filter
def readfile(filename):
    with open(filename) as rf:
        return rf.read()
