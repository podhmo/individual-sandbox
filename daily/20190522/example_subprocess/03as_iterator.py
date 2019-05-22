import time
import subprocess


cmd = [
    "python",
    "-u",
    "-c",
    "import time; [print(i) or time.sleep(0.4) for i in range(10)]",
]
with subprocess.Popen(cmd, text=True, stdout=subprocess.PIPE) as p:
    for line in iter(p.stdout.readline, ""):
        print("* ", time.time(), line.rstrip())
