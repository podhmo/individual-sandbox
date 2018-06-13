import os

print(os.stat("./a.csv").st_mtime)
print(os.utime("./a.csv"))
