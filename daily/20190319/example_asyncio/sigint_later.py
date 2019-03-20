import signal
import subprocess
import sys
from handofcats import as_command


@as_command
def run(file: str, n: int = 2):
    print(f" start python {file} (timeout={n})")
    p = subprocess.Popen(["python", file], stdout=sys.stdout, stderr=subprocess.STDOUT)
    for i in range(4):
        try:
            p.wait(timeout=n)
        except subprocess.TimeoutExpired as e:
            print(e, file=sys.stderr)
            if i < 3:
                print("SIGINT!!")
                p.send_signal(signal.SIGINT)
            else:
                print("SIGKILL!!")
                p.kill()
    print("finish", "python", file)
