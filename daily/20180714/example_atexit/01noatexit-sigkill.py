try:
    print("start")
    import os
    import subprocess
    import signal
    pid = os.getpid()
    subprocess.run(f"kill {signal.SIGHUP.value} -p {pid}")
    print("end")
finally:
    print("finally")
