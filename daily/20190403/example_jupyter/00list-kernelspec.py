import logging
from jupyter_client.kernelspecapp import ListKernelSpecs

logging.basicConfig(level=logging.DEBUG)

app = ListKernelSpecs()
app.start()
# -- stdout --------------------
# >> Available kernels:
# >>   python3    /home/nao/venvs/my/share/jupyter/kernels/python3
