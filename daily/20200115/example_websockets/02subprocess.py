import subprocess

# import concurrent.futures

p0 = subprocess.Popen(["python", "counter.py"])
p1 = subprocess.Popen(["python", "counter.py"])
p0.wait()
p1.wait()
