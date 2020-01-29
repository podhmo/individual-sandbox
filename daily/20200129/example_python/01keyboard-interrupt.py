import sys
import subprocess
import time
import signal

from handofcats import as_subcommand


@as_subcommand
def run():
    p = subprocess.Popen([sys.executable, __file__, "worker"])
    try:
        for i in range(10):
            print("hmm")
            time.sleep(0.5)
    except KeyboardInterrupt:
        p.send_signal(signal.SIGHUP)
    p.wait()


@as_subcommand
def worker():
    def callback(sig, tb):
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        print(sig, tb)
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

    try:
        signal.signal(signal.SIGHUP, callback)
        for i in range(10):
            print(i)
            time.sleep(0.5)
    except KeyboardInterrupt:
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        print("keyboard interrupt")
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")


as_subcommand.run()
