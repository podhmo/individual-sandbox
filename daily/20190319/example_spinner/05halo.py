import time
from halo import Halo

spinner = Halo(text="Loading", spinner="dots")
spinner.start()

time.sleep(2)

spinner.stop()
