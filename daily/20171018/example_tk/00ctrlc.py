import tkinter as tk
import threading
import signal
import sys


class MyTkApp(threading.Thread):
    def run(self):
        self.root = tk.Tk()
        self.root.mainloop()


app = MyTkApp()
app.start()


def signal_handler(signal, frame):
    sys.stderr.write("Exiting...\n")

    # think only one of these is needed, not sure
    app.root.destroy()
    app.root.quit()


signal.signal(signal.SIGINT, signal_handler)
print("end")
