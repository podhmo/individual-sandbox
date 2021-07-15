import sys
import traceback


def run():
    print(">>>", sys.excepthook)
    print(">>>", sys.excepthook(Exception, Exception("hmm"), traceback.extract_stack()))
    raise Exception("oops")


run()
