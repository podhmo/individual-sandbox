import halo
import time

spinner = halo.Halo(text="loading")
spinner.start()
time.sleep(3)
spinner.stop()
spinner.succeed("OK")
