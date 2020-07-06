import runpy

m = runpy.run_path("config.py")
print(m["Config"])
