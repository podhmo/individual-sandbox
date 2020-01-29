from minitask.communication import fake
import minitask

ipc = minitask.IPC(communication=fake)

with ipc.serve("xxx") as x:
    x.send("hello")
    x.send("hello")
    x.send("hello")

with ipc.connect("xxx") as x:
    for i, item in enumerate(x):
        if i == 2:
            break
        print(i, item)
print("end")
