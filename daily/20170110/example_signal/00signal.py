import signal


def rollback(signum, _):
    print("@@", signum)

signal.signal(signal.SIGHUP, rollback)
