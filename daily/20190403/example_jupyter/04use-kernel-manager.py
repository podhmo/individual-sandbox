import pathlib
from jupyter_client.manager import KernelManager
from jupyter_client.connect import find_connection_file

# forkする？
m = KernelManager()
m.connection_file = "kernel-me.json"
m.write_connection_file()
info = m.get_connection_info()
print(info)
print(m.connection_file)

print(find_connection_file())
print(find_connection_file(pathlib.Path(m.connection_file).name))
# m.load_connection_file()
m.blocking_client()
