from IPython import get_ipython
from IPython import InteractiveShell

shell = InteractiveShell.instance()

ip = get_ipython()
ip.run_cell("%time None")
# CPU times: user 2 µs, sys: 0 ns, total: 2 µs
# Wall time: 3.58 µs
