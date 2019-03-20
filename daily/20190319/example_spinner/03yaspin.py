import time
from yaspin import yaspin
from yaspin.spinners import Spinners

with yaspin(Spinners.earth, text="Earth") as sp:
    time.sleep(2)  # time consuming code

    # change spinner
    sp.spinner = Spinners.moon
    sp.text = "Moon"

    time.sleep(2)
