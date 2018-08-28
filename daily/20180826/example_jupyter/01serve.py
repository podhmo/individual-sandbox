from jupyter_client.kernelapp import KernelApp
from traitlets import Any


# TODO: https://github.com/jupyter/notebook/issues/2503
# http://www.tornadoweb.org/en/stable/netutil.html#tornado.netutil.bind_unix_socket
class MyKernelApp(KernelApp):
    connection_name = Any(help="connection_filename").tag(config=True)
    aliases = {
        "connection-name": "MyKernelApp.connection_name",
        **KernelApp.aliases,
    }

    def initialize(self, argv=None):
        super().initialize(argv=argv)
        if self.kernel_name is None:
            self.kernel_name = "python"
        if self.connection_name is not None:
            self.km.connection_file = self.connection_name
        self.km.transport = "ipc"


if __name__ == "__main__":
    MyKernelApp.launch_instance()
