import subprocess

p = subprocess.Popen(["python", "00echo.py"])
print("@", p)
p.wait()
print("@@", p)
subprocess.Popen(["ls", "-l"]).wait()

