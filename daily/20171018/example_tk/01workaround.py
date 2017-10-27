import sys
import signal
import tkinter as tk
import threading


class MyTkApp(threading.Thread):
    def run(self):
        self.root = tk.Tk()
        self.root.mainloop()


app = MyTkApp()
app.start()


def signal_handler(signal, frame):
    import os
    sys.stderr.write("Exiting...\n")
    os.kill(os.getpid(), 9)


signal.signal(signal.SIGINT, signal_handler)
