import time
from yaspin import yaspin

with yaspin().white.bold.shark.on_blue as sp:
    sp.text = "White bold shark in a blue sea"
    time.sleep(5)
