from jupyter_client.kernelapp import KernelApp

app = KernelApp(kernel="python")
app.initialize()
print("config::", app.config)
print("file::", app.km.connection_file)
print(type(app.km.get_connection_info()), app.km.get_connection_info())
app.start()
# get_connection_info
