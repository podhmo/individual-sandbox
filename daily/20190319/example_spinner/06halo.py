import time
from halo import Halo

spinner = Halo(text="Loading")
spinner.start()

time.sleep(2)

spinner.stop()
