import msvcrt
from time import sleep

# from https://stackoverflow.com/questions/61471421/how-do-i-detect-ctrlkey-in-python-without-having-to-download-other-modules

while 1:
    if msvcrt.kbhit():
        key = msvcrt.getch()
        # print(key) # uncomment to see which keys are being pressed.
        if key == b"\x13":
            print("CTRL+S")

    sleep(0.05) # Added to reduce cpu load, 5% before, 0.01% after
