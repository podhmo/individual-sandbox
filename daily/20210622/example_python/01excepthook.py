import sys
import traceback
import json


def run():
    print(">>>", sys.excepthook)
    print(">>>", sys.excepthook(Exception, Exception("hmm"), None))
    raise Exception("oops")


def excepthook(typ, value, tb):
    print("")
    print("----------------------------------------")
    print(
        json.dumps(
            {
                "type": repr(typ),
                "value": repr(value),
                "traceback": [
                    line
                    for lines in traceback.extract_tb(tb).format()
                    for line in lines.splitlines()
                ],
            },
            indent=2,
        )
    )
    print("----------------------------------------")
    print("")


assert sys.__excepthook__ == sys.excepthook
sys.excepthook = excepthook
run()
