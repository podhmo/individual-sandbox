import sys
import subprocess
import time
import signal

from handofcats import as_subcommand


@as_subcommand
def run():
    p = subprocess.Popen([sys.executable, __file__, "worker"])
    time.sleep(0.5)
    p.send_signal(signal.SIGHUP)
    p.wait()


@as_subcommand
def worker():
    def callback(sig, tb):
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
        print(sig, tb)
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

    signal.signal(signal.SIGHUP, callback)
    for i in range(10):
        print(i)
        time.sleep(0.5)


as_subcommand.run()
